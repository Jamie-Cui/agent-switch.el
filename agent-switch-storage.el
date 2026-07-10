;;; agent-switch-storage.el --- Versioned JSON storage and file safety -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Versioned per-Profile JSON storage, state persistence, secret references,
;; optimistic concurrency, and atomic file helpers for agent-switch.

;;; Code:

(require 'auth-source)
(require 'json)
(require 'map)
(require 'rx)
(require 'seq)
(require 'subr-x)
(require 'agent-switch-core)

(defcustom agent-switch-directory
  (expand-file-name "agent-switch/" user-emacs-directory)
  "Directory containing managed profiles and state."
  :type 'directory
  :group 'agent-switch)

(defconst agent-switch-storage-schema-version 1
  "Current Profile and state JSON schema version.")

(defconst agent-switch-json-null :null)
(defconst agent-switch-json-false :false)

(cl-defstruct (agent-switch-file-state
               (:constructor agent-switch--make-file-state))
  path exists-p content hash)

(cl-defstruct (agent-switch-state-record
               (:constructor agent-switch--make-state-record))
  data hash error)

(defvar agent-switch--discovery-cache (make-hash-table :test #'equal)
  "Asynchronous Adapter discovery results keyed by Client ID.")

(defun agent-switch--directory ()
  "Return normalized `agent-switch-directory'."
  (file-name-as-directory (expand-file-name agent-switch-directory)))

(defun agent-switch-profiles-directory (&optional client-id)
  "Return profiles directory, optionally scoped to CLIENT-ID."
  (let ((root (expand-file-name "profiles/" (agent-switch--directory))))
    (if client-id
        (expand-file-name
         (file-name-as-directory
          (agent-switch--string-id client-id "client"))
         root)
      root)))

(defun agent-switch-state-path ()
  "Return the state JSON path."
  (expand-file-name "state.json" (agent-switch--directory)))

(defun agent-switch-profile-path (client-id profile-id)
  "Return managed profile path for CLIENT-ID and PROFILE-ID."
  (expand-file-name
   (concat (agent-switch--string-id profile-id "profile") ".json")
   (agent-switch-profiles-directory client-id)))

(defun agent-switch--read-file-text (path)
  "Read and decode PATH as text."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun agent-switch--read-file-bytes (path)
  "Read PATH literally and return its exact bytes."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path)
    (buffer-string)))

(defun agent-switch-content-hash (content)
  "Return a SHA-256 hash for CONTENT."
  (secure-hash 'sha256 content))

(defun agent-switch-capture-file (path)
  "Capture PATH content and hash for optimistic writes or rollback."
  (let ((exists-p (file-exists-p path)))
    (if exists-p
        (let ((content (agent-switch--read-file-bytes path)))
          (agent-switch--make-file-state
           :path path :exists-p t :content content
           :hash (agent-switch-content-hash content)))
      (agent-switch--make-file-state
       :path path :exists-p nil :content nil :hash :missing))))

(defun agent-switch--current-file-hash (path)
  "Return current hash for PATH, or `:missing'."
  (if (file-exists-p path)
      (agent-switch-content-hash (agent-switch--read-file-bytes path))
    :missing))

(defun agent-switch--assert-file-hash (path expected-hash)
  "Signal a conflict unless PATH still has EXPECTED-HASH."
  (unless (equal (agent-switch--current-file-hash path) expected-hash)
    (signal 'agent-switch-conflict
            (list (format "File changed externally: %s"
                          (abbreviate-file-name path))))))

(defun agent-switch-write-text-atomic (path text expected-hash &optional create-parent)
  "Atomically write TEXT to PATH if it still has EXPECTED-HASH.
When CREATE-PARENT is non-nil, create the parent directory."
  (let ((directory (file-name-directory path)))
    (when create-parent
      (make-directory directory t))
    (unless (file-directory-p directory)
      (signal 'agent-switch-error
              (list (format "Parent directory does not exist: %s"
                            (abbreviate-file-name directory)))))
    (agent-switch--assert-file-hash path expected-hash)
    (let ((temporary
           (make-temp-file
            (expand-file-name
             (concat "." (file-name-nondirectory path) ".tmp-") directory))))
      (unwind-protect
          (progn
            (let ((coding-system-for-write
                   (if (multibyte-string-p text) 'utf-8-unix 'no-conversion)))
              (with-temp-file temporary (insert text)))
            (agent-switch--assert-file-hash path expected-hash)
            (rename-file temporary path t)
            (agent-switch--current-file-hash path))
        (when (file-exists-p temporary)
          (ignore-errors (delete-file temporary)))))))

(defun agent-switch-delete-file-optimistic (path expected-hash)
  "Delete PATH only if it still has EXPECTED-HASH."
  (agent-switch--assert-file-hash path expected-hash)
  (when (file-exists-p path)
    (delete-file path)))

(defun agent-switch-backup-file (path)
  "Create and return a timestamped backup of PATH, or nil if absent."
  (when (file-exists-p path)
    (let* ((stamp (format-time-string "%Y%m%dT%H%M%S"))
           (backup (format "%s.agent-switch.bak.%s" path stamp))
           (candidate backup)
           (counter 1))
      (while (file-exists-p candidate)
        (setq candidate (format "%s.%d" backup counter)
              counter (1+ counter)))
      (copy-file path candidate nil t t)
      candidate)))

(defun agent-switch-restore-file (state)
  "Restore file STATE captured by `agent-switch-capture-file'."
  (let ((path (agent-switch-file-state-path state)))
    (if (agent-switch-file-state-exists-p state)
        (let ((current-hash (agent-switch--current-file-hash path)))
          (agent-switch-write-text-atomic
           path (agent-switch-file-state-content state) current-hash t))
      (when (file-exists-p path)
        (delete-file path)))))

(defun agent-switch-parse-json (text &optional context)
  "Parse JSON TEXT as hash-table data.
CONTEXT is included in sanitized parse errors."
  (condition-case nil
      (json-parse-string text
                         :object-type 'hash-table
                         :array-type 'array
                         :null-object agent-switch-json-null
                         :false-object agent-switch-json-false)
    (json-parse-error
     (signal 'agent-switch-validation-error
             (list (format "Invalid JSON in %s" (or context "data")))))))

(defun agent-switch-json-serialize (value)
  "Serialize JSON VALUE in a stable human-readable form."
  (with-temp-buffer
    (insert (json-serialize value
                            :null-object agent-switch-json-null
                            :false-object agent-switch-json-false))
    (json-pretty-print-buffer)
    (unless (bolp) (insert "\n"))
    (buffer-string)))

(defun agent-switch-json-copy (value)
  "Return a deep copy of JSON VALUE."
  (cond
   ((hash-table-p value)
    (let ((copy (make-hash-table :test #'equal)))
      (maphash (lambda (key child)
                 (puthash key (agent-switch-json-copy child) copy))
               value)
      copy))
   ((vectorp value)
    (vconcat (mapcar #'agent-switch-json-copy (append value nil))))
   ((consp value) (mapcar #'agent-switch-json-copy value))
   (t value)))

(defun agent-switch-json-deep-merge (target patch)
  "Deeply merge JSON PATCH into TARGET and return a copy."
  (if (and (hash-table-p target) (hash-table-p patch))
      (let ((result (agent-switch-json-copy target)))
        (maphash
         (lambda (key value)
           (puthash key
                    (agent-switch-json-deep-merge
                     (gethash key result) value)
                    result))
         patch)
        result)
    (agent-switch-json-copy patch)))

(defun agent-switch-json-get-in (object path &optional default)
  "Return nested OBJECT value at string key PATH, or DEFAULT."
  (let ((value object)
        (missing (make-symbol "missing")))
    (catch 'missing
      (dolist (key path)
        (unless (hash-table-p value)
          (throw 'missing default))
        (setq value (gethash key value missing))
        (when (eq value missing)
          (throw 'missing default)))
      value)))

(defun agent-switch-json-put-in (object path value)
  "Set nested OBJECT string key PATH to VALUE and return OBJECT."
  (unless path
    (signal 'agent-switch-validation-error '("JSON path cannot be empty")))
  (let ((cursor object))
    (dolist (key (butlast path))
      (let ((child (gethash key cursor)))
        (unless (hash-table-p child)
          (setq child (make-hash-table :test #'equal))
          (puthash key child cursor))
        (setq cursor child)))
    (puthash (car (last path)) value cursor))
  object)

(defun agent-switch-json-remove-in (object path)
  "Remove nested key PATH from OBJECT."
  (let ((cursor object))
    (dolist (key (butlast path))
      (setq cursor (and (hash-table-p cursor) (gethash key cursor))))
    (when (hash-table-p cursor)
      (remhash (car (last path)) cursor)))
  object)

(defun agent-switch-secret-reference-p (value)
  "Return non-nil when VALUE is a supported secret reference object."
  (and (hash-table-p value)
       (let ((source (gethash "source" value)))
         (cond
          ((equal source "env")
           (let ((name (gethash "name" value)))
             (and (stringp name)
                  (string-match-p "\\`[[:alpha:]_][[:alnum:]_]*\\'" name))))
          ((equal source "auth-source")
           (let ((host (gethash "host" value)))
             (and (stringp host) (not (string-empty-p host)))))
          (t nil)))))

(defconst agent-switch--sensitive-key-regexp
  (rx (or "token" "secret" "password" "api-key" "api_key" "apikey"
          "authorization" "auth-token" "auth_token"))
  "Case-insensitive regexp identifying secret-bearing JSON keys.")

(defun agent-switch-validate-no-plaintext-secrets (value &optional path)
  "Signal if JSON VALUE contains a plaintext secret.
PATH is used internally to identify sensitive keys without exposing values."
  (cond
   ((hash-table-p value)
    (maphash
     (lambda (key child)
       (let ((child-path (append path (list key))))
         (when (and (stringp key)
                    (string-match-p agent-switch--sensitive-key-regexp
                                    (downcase key))
                    (not (agent-switch-secret-reference-p child))
                    (not (or (eq child agent-switch-json-null)
                             (eq child agent-switch-json-false)
                             (null child))))
           (signal 'agent-switch-validation-error
                   (list (format "Plaintext secret is not allowed at %s"
                                 (string-join child-path ".")))))
         (unless (agent-switch-secret-reference-p child)
           (agent-switch-validate-no-plaintext-secrets child child-path))))
     value))
   ((vectorp value)
    (dotimes (index (length value))
      (agent-switch-validate-no-plaintext-secrets
       (aref value index)
       (append path (list (number-to-string index))))))
   ((consp value)
    (dolist (child value)
      (agent-switch-validate-no-plaintext-secrets child path))))
  t)

(defun agent-switch--resolve-secret-reference (reference)
  "Resolve secret REFERENCE or signal a sanitized error."
  (let ((source (gethash "source" reference)))
    (pcase source
      ("env"
       (let* ((name (gethash "name" reference))
              (value (getenv name)))
         (unless (and value (not (string-empty-p value)))
           (signal 'agent-switch-error
                   (list (format "Environment variable %s is not set" name))))
         value))
      ("auth-source"
       (let* ((host (gethash "host" reference))
              (user (gethash "user" reference))
              (port (gethash "port" reference))
              (match (car (apply #'auth-source-search
                                 (append (list :host host :max 1 :require '(:secret))
                                         (when user (list :user user))
                                         (when port (list :port port))))))
              (secret (plist-get match :secret))
              (value (if (functionp secret) (funcall secret) secret)))
         (unless (and (stringp value) (not (string-empty-p value)))
           (signal 'agent-switch-error
                   (list (format "No auth-source secret found for %s" host))))
         value))
      (_ (signal 'agent-switch-validation-error
                 '("Unsupported secret reference"))))))

(defun agent-switch--resolve-secrets (value secrets)
  "Return (RESOLVED . SECRETS) for JSON VALUE and accumulated SECRETS."
  (cond
   ((agent-switch-secret-reference-p value)
    (let ((secret (agent-switch--resolve-secret-reference value)))
      (cons secret (cons secret secrets))))
   ((hash-table-p value)
    (let ((copy (make-hash-table :test #'equal))
          (values secrets))
      (maphash
       (lambda (key child)
         (pcase-let ((`(,resolved . ,new-values)
                      (agent-switch--resolve-secrets child values)))
           (setq values new-values)
           (puthash key resolved copy)))
       value)
      (cons copy values)))
   ((vectorp value)
    (let ((copy (make-vector (length value) nil))
          (values secrets))
      (dotimes (index (length value))
        (pcase-let ((`(,resolved . ,new-values)
                     (agent-switch--resolve-secrets (aref value index) values)))
          (setq values new-values)
          (aset copy index resolved)))
      (cons copy values)))
   ((consp value)
    (let (copy (values secrets))
      (dolist (child value)
        (pcase-let ((`(,resolved . ,new-values)
                     (agent-switch--resolve-secrets child values)))
          (setq values new-values)
          (push resolved copy)))
      (cons (nreverse copy) values)))
   (t (cons value secrets))))

(defun agent-switch-resolve-profile-secrets (profile)
  "Return (RESOLVED-PROFILE . SECRET-VALUES) for PROFILE."
  (pcase-let ((`(,payload . ,secrets)
               (agent-switch--resolve-secrets
                (agent-switch-profile-payload profile) nil)))
    (cons (let ((copy (copy-agent-switch-profile profile)))
            (setf (agent-switch-profile-payload copy) payload)
            copy)
          secrets)))

(defun agent-switch--profile-json (profile)
  "Return versioned JSON object for managed PROFILE."
  (let ((object (make-hash-table :test #'equal)))
    (puthash "schema_version" agent-switch-storage-schema-version object)
    (puthash "id" (agent-switch-profile-id profile) object)
    (puthash "client" (agent-switch-profile-client-id profile) object)
    (puthash "name" (agent-switch-profile-name profile) object)
    (when-let* ((description (agent-switch-profile-description profile)))
      (puthash "description" description object))
    (puthash "payload" (agent-switch-profile-payload profile) object)
    object))

(defun agent-switch--profile-from-json (path client-id object hash)
  "Build a managed profile from PATH, CLIENT-ID, OBJECT, and HASH."
  (let ((version (gethash "schema_version" object))
        (id (gethash "id" object))
        (stored-client (gethash "client" object))
        (name (gethash "name" object))
        (payload (gethash "payload" object)))
    (unless (equal version agent-switch-storage-schema-version)
      (signal 'agent-switch-validation-error
              (list (format "Unsupported profile schema version: %S" version))))
    (setq id (agent-switch--string-id id "profile"))
    (unless (equal (file-name-base path) id)
      (signal 'agent-switch-validation-error
              '("Profile filename does not match its ID")))
    (unless (equal stored-client client-id)
      (signal 'agent-switch-validation-error
              '("Profile belongs to another client")))
    (unless (and (stringp name) (not (string-empty-p (string-trim name))))
      (signal 'agent-switch-validation-error '("Profile name is required")))
    (unless (hash-table-p payload)
      (signal 'agent-switch-validation-error
              '("Profile payload must be a JSON object")))
    (agent-switch-validate-no-plaintext-secrets payload)
    (agent-switch--make-profile
     :id id :client-id client-id :name name
     :description (gethash "description" object)
     :payload payload :ownership 'managed :source path :source-hash hash
     :valid-p t)))

(defun agent-switch--invalid-profile (path client-id error-value hash)
  "Return an invalid profile for PATH and CLIENT-ID.
ERROR-VALUE is sanitized for display and HASH records the source content."
  (agent-switch--make-profile
   :id (file-name-base path)
   :client-id client-id
   :name (file-name-base path)
   :description nil
   :payload (make-hash-table :test #'equal)
   :ownership 'managed
   :source path
   :source-hash hash
   :valid-p nil
   :error (agent-switch--safe-error-message error-value)))

(defun agent-switch-load-managed-profiles (client-id)
  "Load managed profiles for CLIENT-ID, isolating per-file errors."
  (setq client-id (agent-switch--string-id client-id "client"))
  (let ((directory (agent-switch-profiles-directory client-id))
        profiles)
    (when (file-directory-p directory)
      (dolist (path (directory-files directory t "\\.json\\'" t))
        (let* ((text (agent-switch--read-file-text path))
               (hash (agent-switch-content-hash
                      (agent-switch--read-file-bytes path))))
          (push
           (condition-case error-value
               (agent-switch--profile-from-json
                path client-id
                (agent-switch-parse-json text (file-name-nondirectory path))
                hash)
             (error (agent-switch--invalid-profile
                     path client-id error-value hash)))
           profiles))))
    (sort profiles
          (lambda (left right)
            (string-lessp (agent-switch-profile-id left)
                          (agent-switch-profile-id right))))))

(defun agent-switch--adapter-discovered-profiles (client)
  "Return Adapter-discovered profiles for CLIENT.
Asynchronous discovery is cached and announces completion through
`agent-switch-data-changed-hook'."
  (let* ((adapter (agent-switch-get-adapter
                   (agent-switch-client-adapter-id client)))
         (discover (agent-switch-adapter-callback adapter :discover))
         (client-id (agent-switch-client-id client))
         (cached (gethash client-id agent-switch--discovery-cache)))
    (when discover
      (pcase (plist-get cached :status)
        ('ready (plist-get cached :value))
        ('pending nil)
        ('error
         (signal 'agent-switch-error (list (plist-get cached :error))))
        (_
         (let ((result (funcall discover client nil)))
           (if (not (agent-switch-job-p result))
               result
             (puthash client-id (list :status 'pending :job result)
                      agent-switch--discovery-cache)
             (agent-switch-job-start
              result
              (lambda (profiles)
                (puthash client-id (list :status 'ready :value profiles)
                         agent-switch--discovery-cache)
                (run-hook-with-args 'agent-switch-data-changed-hook client-id))
              (lambda (error-value)
                (puthash client-id
                         (list :status 'error
                               :error (agent-switch--safe-error-message error-value))
                         agent-switch--discovery-cache)
                (run-hook-with-args 'agent-switch-data-changed-hook client-id)))
             nil)))))))

(defun agent-switch-invalidate-discovery (&optional client-id)
  "Invalidate asynchronous discovery cache for CLIENT-ID or all clients."
  (if client-id
      (progn
        (when-let* ((entry (gethash client-id agent-switch--discovery-cache))
                    (job (and (eq (plist-get entry :status) 'pending)
                              (plist-get entry :job))))
          (agent-switch-job-cancel job))
        (remhash client-id agent-switch--discovery-cache))
    (maphash (lambda (_id entry)
               (when-let* ((job (and (eq (plist-get entry :status) 'pending)
                                     (plist-get entry :job))))
                 (agent-switch-job-cancel job)))
             agent-switch--discovery-cache)
    (clrhash agent-switch--discovery-cache)))

(defun agent-switch--ordered-profiles (client-id profiles)
  "Order PROFILES for CLIENT-ID using state preferences."
  (let* ((order (agent-switch-state-profile-order client-id))
         (index (make-hash-table :test #'equal)))
    (cl-loop for id in order for position from 0
             do (puthash id position index))
    (sort profiles
          (lambda (left right)
            (let ((left-index (gethash (agent-switch-profile-id left) index))
                  (right-index (gethash (agent-switch-profile-id right) index)))
              (cond
               ((and left-index right-index) (< left-index right-index))
               (left-index t)
               (right-index nil)
               (t (string-lessp (agent-switch-profile-id left)
                                 (agent-switch-profile-id right)))))))))

(defun agent-switch-profiles (client-id)
  "Return managed and external profiles for CLIENT-ID in display order."
  (let* ((client (agent-switch-get-client client-id))
         (profiles (append (agent-switch-load-managed-profiles client-id)
                           (agent-switch-external-profiles client-id)
                           (agent-switch--adapter-discovered-profiles client))))
    (agent-switch--ordered-profiles client-id profiles)))

(defun agent-switch-find-profile (client-id profile-id &optional noerror)
  "Return CLIENT-ID PROFILE-ID.
When NOERROR is non-nil, return nil on absence."
  (or (cl-find profile-id (agent-switch-profiles client-id)
               :key #'agent-switch-profile-id :test #'equal)
      (unless noerror
        (signal 'agent-switch-error
                (list (format "Unknown profile %s/%s" client-id profile-id))))))

(defun agent-switch-save-profile (profile)
  "Validate and atomically save managed PROFILE."
  (unless (eq (agent-switch-profile-ownership profile) 'managed)
    (signal 'agent-switch-validation-error
            '("Only managed profiles can be saved")))
  (let* ((client-id (agent-switch--string-id
                     (agent-switch-profile-client-id profile) "client"))
         (id (agent-switch--string-id
              (agent-switch-profile-id profile) "profile"))
         (path (agent-switch-profile-path client-id id))
         (expected (or (agent-switch-profile-source-hash profile) :missing)))
    (unless (and (stringp (agent-switch-profile-name profile))
                 (not (string-empty-p
                       (string-trim (agent-switch-profile-name profile)))))
      (signal 'agent-switch-validation-error '("Profile name is required")))
    (unless (hash-table-p (agent-switch-profile-payload profile))
      (signal 'agent-switch-validation-error
              '("Profile payload must be a JSON object")))
    (agent-switch-validate-no-plaintext-secrets
     (agent-switch-profile-payload profile))
    (let ((hash (agent-switch-write-text-atomic
                 path
                 (agent-switch-json-serialize
                  (agent-switch--profile-json profile))
                 expected t)))
      (setf (agent-switch-profile-source profile) path
            (agent-switch-profile-source-hash profile) hash
            (agent-switch-profile-valid-p profile) t)
      profile)))

(defun agent-switch-delete-profile (profile)
  "Delete managed PROFILE using optimistic concurrency."
  (unless (eq (agent-switch-profile-ownership profile) 'managed)
    (signal 'agent-switch-validation-error
            '("External profiles cannot be deleted")))
  (agent-switch-delete-file-optimistic
   (agent-switch-profile-source profile)
   (agent-switch-profile-source-hash profile))
  (agent-switch-state-remove-profile
   (agent-switch-profile-client-id profile)
   (agent-switch-profile-id profile)))

(defun agent-switch--empty-state ()
  "Return a new versioned state object."
  (let ((object (make-hash-table :test #'equal)))
    (puthash "schema_version" agent-switch-storage-schema-version object)
    (puthash "last_selected" (make-hash-table :test #'equal) object)
    (puthash "profile_order" (make-hash-table :test #'equal) object)
    (puthash "unprotected_confirmed" [] object)
    (puthash "canonical_confirmations" (make-hash-table :test #'equal) object)
    object))

(defun agent-switch-read-state ()
  "Read state and return an `agent-switch-state-record'."
  (let ((path (agent-switch-state-path)))
    (if (not (file-exists-p path))
        (agent-switch--make-state-record
         :data (agent-switch--empty-state) :hash :missing)
      (let* ((text (agent-switch--read-file-text path))
             (hash (agent-switch-content-hash
                    (agent-switch--read-file-bytes path))))
        (condition-case error-value
            (let ((data (agent-switch-parse-json text "state.json")))
              (unless (equal (gethash "schema_version" data)
                             agent-switch-storage-schema-version)
                (signal 'agent-switch-validation-error
                        '("Unsupported state schema version")))
              (agent-switch--make-state-record :data data :hash hash))
          (error
           (agent-switch--make-state-record
            :data (agent-switch--empty-state)
            :hash hash
            :error (agent-switch--safe-error-message error-value))))))))

(defun agent-switch-update-state (mutator)
  "Apply MUTATOR to state and atomically persist it."
  (let* ((record (agent-switch-read-state))
         (error-text (agent-switch-state-record-error record)))
    (when error-text
      (signal 'agent-switch-validation-error
              (list (concat "state.json is damaged; reset it before writing: "
                            error-text))))
    (let ((data (agent-switch-json-copy
                 (agent-switch-state-record-data record))))
      (funcall mutator data)
      (agent-switch-write-text-atomic
       (agent-switch-state-path)
       (agent-switch-json-serialize data)
       (agent-switch-state-record-hash record) t)
      data)))

(defun agent-switch-reset-state ()
  "Back up a damaged or valid state file and replace it with empty state."
  (interactive)
  (let* ((path (agent-switch-state-path))
         (expected (agent-switch--current-file-hash path))
         (backup (agent-switch-backup-file path)))
    (agent-switch-write-text-atomic
     path (agent-switch-json-serialize (agent-switch--empty-state))
     expected t)
    (when (called-interactively-p 'interactive)
      (message "Reset agent-switch state%s"
               (if backup
                   (format "; backup: %s" (abbreviate-file-name backup))
                 "")))
    backup))

(defun agent-switch-state-last-selected (client-id)
  "Return last selected Profile ID for CLIENT-ID."
  (let* ((data (agent-switch-state-record-data (agent-switch-read-state)))
         (last-selected (gethash "last_selected" data)))
    (and (hash-table-p last-selected)
         (gethash client-id last-selected))))

(defun agent-switch-state-set-last-selected (client-id profile-id)
  "Record PROFILE-ID as last selected for CLIENT-ID."
  (agent-switch-update-state
   (lambda (data)
     (let ((table (or (gethash "last_selected" data)
                      (let ((new (make-hash-table :test #'equal)))
                        (puthash "last_selected" new data)
                        new))))
       (puthash client-id profile-id table)))))

(defun agent-switch-state-profile-order (client-id)
  "Return saved profile order for CLIENT-ID."
  (let* ((data (agent-switch-state-record-data (agent-switch-read-state)))
         (orders (gethash "profile_order" data))
         (order (and (hash-table-p orders) (gethash client-id orders))))
    (cond ((vectorp order) (append order nil))
          ((listp order) order)
          (t nil))))

(defun agent-switch-state-set-profile-order (client-id profile-ids)
  "Persist PROFILE-IDS display order for CLIENT-ID."
  (agent-switch-update-state
   (lambda (data)
     (let ((orders (or (gethash "profile_order" data)
                       (let ((new (make-hash-table :test #'equal)))
                         (puthash "profile_order" new data)
                         new))))
       (puthash client-id (vconcat profile-ids) orders)))))

(defun agent-switch-state-remove-profile (client-id profile-id)
  "Remove PROFILE-ID references for CLIENT-ID from state."
  (agent-switch-update-state
   (lambda (data)
     (let ((orders (gethash "profile_order" data))
           (last-selected (gethash "last_selected" data)))
       (when (hash-table-p orders)
         (let ((order (gethash client-id orders)))
           (puthash client-id
                    (vconcat (delete profile-id (append order nil)))
                    orders)))
       (when (and (hash-table-p last-selected)
                  (equal (gethash client-id last-selected) profile-id))
         (remhash client-id last-selected))))))

(defun agent-switch-state-unprotected-confirmed-p (adapter-id)
  "Return non-nil if ADAPTER-ID activation risk was confirmed."
  (let* ((data (agent-switch-state-record-data (agent-switch-read-state)))
         (confirmed (gethash "unprotected_confirmed" data)))
    (member adapter-id (append confirmed nil))))

(defun agent-switch-state-confirm-unprotected (adapter-id)
  "Record confirmation for ADAPTER-ID without rollback support."
  (agent-switch-update-state
   (lambda (data)
     (let* ((old (append (gethash "unprotected_confirmed" data) nil))
            (new (vconcat (cl-adjoin adapter-id old :test #'equal))))
       (puthash "unprotected_confirmed" new data)))))

(defun agent-switch-state-canonical-confirmed-p (key hash)
  "Return non-nil when canonical rewrite KEY was confirmed for HASH."
  (let* ((data (agent-switch-state-record-data (agent-switch-read-state)))
         (table (gethash "canonical_confirmations" data)))
    (and (hash-table-p table) (equal (gethash key table) hash))))

(defun agent-switch-state-confirm-canonical (key hash)
  "Record canonical rewrite KEY confirmation for source HASH."
  (agent-switch-update-state
   (lambda (data)
     (let ((table (or (gethash "canonical_confirmations" data)
                      (let ((new (make-hash-table :test #'equal)))
                        (puthash "canonical_confirmations" new data)
                        new))))
       (puthash key hash table)))))

(provide 'agent-switch-storage)

;;; agent-switch-storage.el ends here
