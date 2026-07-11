;;; agent-switch-core.el --- Domain model and adapter protocol -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Public domain objects, registries, Job handling, and transactional Adapter
;; activation for agent-switch.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defgroup agent-switch nil
  "Manage provider profiles for LLM agent clients."
  :group 'tools
  :prefix "agent-switch-")

(define-error 'agent-switch-error "agent-switch error")
(define-error 'agent-switch-conflict "agent-switch file conflict" 'agent-switch-error)
(define-error 'agent-switch-validation-error "agent-switch validation error" 'agent-switch-error)

(cl-defstruct (agent-switch-adapter
               (:constructor agent-switch--make-adapter))
  id name callbacks)

(cl-defstruct (agent-switch-client
               (:constructor agent-switch--make-client))
  id name adapter-id metadata)

(cl-defstruct (agent-switch-profile
               (:constructor agent-switch--make-profile))
  id client-id name description payload ownership source source-hash
  valid-p error metadata)

(cl-defstruct (agent-switch-job
               (:constructor agent-switch-job-create))
  starter canceler description)

(defvar agent-switch--adapters (make-hash-table :test #'equal)
  "Registered adapters keyed by string ID.")

(defvar agent-switch--clients (make-hash-table :test #'equal)
  "Registered clients keyed by string ID.")

(defvar agent-switch--client-order nil
  "Client IDs in registration order.")

(defvar agent-switch--external-profiles (make-hash-table :test #'equal)
  "Elisp-declared external profiles keyed by (CLIENT-ID . PROFILE-ID).")

(defvar agent-switch--secret-values nil
  "Dynamically bound resolved secret strings for error redaction.")

(defvar agent-switch-data-changed-hook nil
  "Hook run after asynchronous Adapter data becomes available.")

(defconst agent-switch--callback-keys
  '(:current :activate :validate :discover :status :snapshot :rollback
    :profile-current-p :capture-current :describe :watch-paths :watch-setup
    :profile-template)
  "Recognized adapter callback keys.")

(defun agent-switch--string-id (value kind)
  "Validate VALUE as a filesystem-safe ID for KIND and return it."
  (let ((id (cond ((stringp value) value)
                  ((symbolp value) (symbol-name value))
                  (t nil))))
    (unless (and id
                 (string-match-p
                  "\\`[[:alnum:]][[:alnum:]_.-]*\\'" id))
      (signal 'agent-switch-validation-error
              (list (format "Invalid %s ID: %S" kind value))))
    id))

(defun agent-switch--safe-error-message (error-value)
  "Return a secret-safe message for ERROR-VALUE."
  (let ((message-text (condition-case nil
                          (error-message-string error-value)
                        (error "Operation failed"))))
    (dolist (secret agent-switch--secret-values)
      (when (and (stringp secret) (not (string-empty-p secret)))
        (setq message-text
              (replace-regexp-in-string
               (regexp-quote secret) "<redacted>" message-text t t))))
    (setq message-text
          (replace-regexp-in-string
           "\\b\\(Bearer[[:space:]]+\\)?\\(sk-[[:alnum:]_-]+\\)"
           "<redacted>" message-text t))
    message-text))

(defun agent-switch-adapter-callback (adapter key &optional required)
  "Return ADAPTER callback KEY.
Signal when REQUIRED is non-nil and the callback is absent."
  (let ((callback (plist-get (agent-switch-adapter-callbacks adapter) key)))
    (when (and required (not (functionp callback)))
      (signal 'agent-switch-error
              (list (format "Adapter %s does not implement %s"
                            (agent-switch-adapter-id adapter) key))))
    callback))

(defun agent-switch-adapter-capability-p (adapter callback-key)
  "Return non-nil when ADAPTER implements CALLBACK-KEY."
  (functionp (agent-switch-adapter-callback adapter callback-key)))

(defun agent-switch-register-adapter (id &rest properties)
  "Register an adapter ID using PROPERTIES.

Required callback properties are `:current' and `:activate'.  Optional
callbacks are listed in `agent-switch--callback-keys'."
  (setq id (agent-switch--string-id id "adapter"))
  (let (callbacks)
    (dolist (key agent-switch--callback-keys)
      (when (plist-member properties key)
        (let ((value (plist-get properties key)))
          (unless (functionp value)
            (signal 'agent-switch-validation-error
                    (list (format "Adapter %s callback %s is not a function"
                                  id key))))
          (setq callbacks (plist-put callbacks key value)))))
    (dolist (required '(:current :activate))
      (unless (functionp (plist-get callbacks required))
        (signal 'agent-switch-validation-error
                (list (format "Adapter %s requires callback %s" id required)))))
    (let ((adapter
           (agent-switch--make-adapter
            :id id
            :name (or (plist-get properties :name) id)
            :callbacks callbacks)))
      (puthash id adapter agent-switch--adapters)
      adapter)))

(defmacro agent-switch-define-adapter (id &rest properties)
  "Define adapter ID declaratively with PROPERTIES."
  (declare (indent 1))
  `(agent-switch-register-adapter ',id ,@properties))

(defun agent-switch-get-adapter (id &optional noerror)
  "Return registered adapter ID.
When NOERROR is nil, signal if it is not registered."
  (let* ((string-id (agent-switch--string-id id "adapter"))
         (adapter (gethash string-id agent-switch--adapters)))
    (or adapter
        (when (not noerror)
          (signal 'agent-switch-error
                  (list (format "Unknown adapter: %s" string-id)))))))

(defun agent-switch-register-client (id &rest properties)
  "Register a managed client ID using PROPERTIES.
`:adapter' is required and may be a symbol or string adapter ID."
  (setq id (agent-switch--string-id id "client"))
  (let* ((adapter-id (agent-switch--string-id
                      (plist-get properties :adapter) "adapter"))
         (_adapter (agent-switch-get-adapter adapter-id))
         (client (agent-switch--make-client
                  :id id
                  :name (or (plist-get properties :name) id)
                  :adapter-id adapter-id
                  :metadata (plist-get properties :metadata))))
    (unless (gethash id agent-switch--clients)
      (setq agent-switch--client-order
            (append agent-switch--client-order (list id))))
    (puthash id client agent-switch--clients)
    client))

(defun agent-switch-get-client (id &optional noerror)
  "Return registered client ID.
When NOERROR is nil, signal if it is not registered."
  (let* ((string-id (agent-switch--string-id id "client"))
         (client (gethash string-id agent-switch--clients)))
    (or client
        (when (not noerror)
          (signal 'agent-switch-error
                  (list (format "Unknown client: %s" string-id)))))))

(defun agent-switch-clients ()
  "Return registered clients in stable registration order."
  (delq nil (mapcar (lambda (id) (gethash id agent-switch--clients))
                    agent-switch--client-order)))

(defun agent-switch-register-profile (client-id id &rest properties)
  "Register an external Elisp profile ID for CLIENT-ID.
PROPERTIES accepts `:name', `:description', `:payload', and `:metadata'."
  (setq client-id (agent-switch--string-id client-id "client"))
  (setq id (agent-switch--string-id id "profile"))
  (agent-switch-get-client client-id)
  (let ((profile (agent-switch--make-profile
                  :id id
                  :client-id client-id
                  :name (or (plist-get properties :name) id)
                  :description (plist-get properties :description)
                  :payload (or (plist-get properties :payload)
                               (make-hash-table :test #'equal))
                  :ownership 'external
                  :source 'elisp
                  :valid-p t
                  :metadata (plist-get properties :metadata))))
    (puthash (cons client-id id) profile agent-switch--external-profiles)
    profile))

(defun agent-switch-external-profiles (client-id)
  "Return Elisp-registered external profiles for CLIENT-ID."
  (setq client-id (agent-switch--string-id client-id "client"))
  (let (profiles)
    (maphash (lambda (key profile)
               (when (equal (car key) client-id)
                 (push profile profiles)))
             agent-switch--external-profiles)
    (sort profiles (lambda (a b)
                     (string-lessp (agent-switch-profile-id a)
                                   (agent-switch-profile-id b))))))

(defun agent-switch-profile-reference (profile)
  "Return stable (CLIENT-ID . PROFILE-ID) reference for PROFILE."
  (cons (agent-switch-profile-client-id profile)
        (agent-switch-profile-id profile)))

(defun agent-switch-job-start (job on-success on-failure)
  "Start JOB, calling ON-SUCCESS or ON-FAILURE exactly once."
  (unless (agent-switch-job-p job)
    (signal 'wrong-type-argument (list 'agent-switch-job-p job)))
  (let ((settled nil))
    (funcall
     (agent-switch-job-starter job)
     (lambda (value)
       (unless settled
         (setq settled t)
         (funcall on-success value)))
     (lambda (error-value)
       (unless settled
         (setq settled t)
         (funcall on-failure error-value))))))

(defun agent-switch-job-cancel (job)
  "Request cancellation of JOB when supported."
  (when-let* ((cancel (and (agent-switch-job-p job)
                           (agent-switch-job-canceler job))))
    (funcall cancel)))

(defun agent-switch--settle-result (thunk on-success on-failure)
  "Run THUNK and settle its value through callbacks.
If THUNK returns an `agent-switch-job', start it.  Call ON-SUCCESS with
the value or ON-FAILURE with the error."
  (condition-case error-value
      (let ((result (funcall thunk)))
        (if (agent-switch-job-p result)
            (agent-switch-job-start result on-success on-failure)
          (funcall on-success result)))
    (error (funcall on-failure error-value))))

(defun agent-switch-call (client callback-key &rest arguments)
  "Call CLIENT adapter CALLBACK-KEY with ARGUMENTS.
The CLIENT object is prepended to ARGUMENTS.  Return a direct value or Job."
  (let* ((adapter (agent-switch-get-adapter
                   (agent-switch-client-adapter-id client)))
         (callback (agent-switch-adapter-callback adapter callback-key t)))
    (apply callback client arguments)))

(defun agent-switch--json-value-equal-p (left right)
  "Return non-nil when JSON-like values LEFT and RIGHT are structurally equal."
  (cond
   ((hash-table-p left)
    (and (hash-table-p right)
         (= (hash-table-count left) (hash-table-count right))
         (let ((equal-p t)
               (missing (make-symbol "missing")))
           (maphash
            (lambda (key value)
              (let ((other (gethash key right missing)))
                (unless (and (not (eq other missing))
                             (agent-switch--json-value-equal-p value other))
                  (setq equal-p nil))))
            left)
           equal-p)))
   ((vectorp left)
    (and (vectorp right)
         (= (length left) (length right))
         (cl-loop for index below (length left)
                  always (agent-switch--json-value-equal-p
                          (aref left index) (aref right index)))))
   ((consp left)
    (and (consp right)
         (agent-switch--json-value-equal-p (car left) (car right))
         (agent-switch--json-value-equal-p (cdr left) (cdr right))))
   (t (equal left right))))

(defun agent-switch-profile-current-p (client profile current &optional context)
  "Return non-nil when PROFILE represents CLIENT CURRENT state."
  (let* ((adapter (agent-switch-get-adapter
                   (agent-switch-client-adapter-id client)))
         (matcher (agent-switch-adapter-callback adapter :profile-current-p)))
    (if matcher
        (funcall matcher client profile current context)
      (agent-switch--json-value-equal-p
       (agent-switch-profile-payload profile) current))))

(defun agent-switch-reset-registries ()
  "Clear all registries.
This is primarily useful to isolate tests and reload built-in adapters."
  (interactive)
  (clrhash agent-switch--adapters)
  (clrhash agent-switch--clients)
  (clrhash agent-switch--external-profiles)
  (setq agent-switch--client-order nil))

;; Storage-owned functions are declared here to keep the activation protocol
;; usable without introducing a circular require.
(declare-function agent-switch-profiles "agent-switch-storage" (client-id))
(declare-function agent-switch-resolve-profile-secrets "agent-switch-storage" (profile))
(declare-function agent-switch-state-last-selected "agent-switch-storage" (client-id))
(declare-function agent-switch-state-set-last-selected "agent-switch-storage"
                  (client-id profile-id &optional profile))
(declare-function agent-switch-state-unprotected-confirmed-p "agent-switch-storage" (adapter-id))
(declare-function agent-switch-state-confirm-unprotected "agent-switch-storage" (adapter-id))

(defun agent-switch--activation-context (client profile interactivep)
  "Create activation context for CLIENT and PROFILE.
INTERACTIVEP records whether a user initiated the operation."
  (list :client-id (agent-switch-client-id client)
        :profile-id (agent-switch-profile-id profile)
        :interactive interactivep
        :started-at (current-time)))

(defun agent-switch-activation-job (client profile &optional interactivep)
  "Return a transactional activation Job for CLIENT and PROFILE.
INTERACTIVEP is recorded in the adapter context."
  (unless (agent-switch-profile-valid-p profile)
    (signal 'agent-switch-validation-error
            (list (or (agent-switch-profile-error profile)
                      "Profile is invalid"))))
  (unless (equal (agent-switch-client-id client)
                 (agent-switch-profile-client-id profile))
    (signal 'agent-switch-validation-error
            (list "Profile belongs to another client")))
  (let* ((adapter (agent-switch-get-adapter
                   (agent-switch-client-adapter-id client)))
         (validate (agent-switch-adapter-callback adapter :validate))
         (snapshot-fn (agent-switch-adapter-callback adapter :snapshot))
         (activate (agent-switch-adapter-callback adapter :activate t))
         (current-fn (agent-switch-adapter-callback adapter :current t))
         (rollback (agent-switch-adapter-callback adapter :rollback))
         (context (agent-switch--activation-context
                   client profile interactivep))
         (cancelled nil)
         (active-child nil)
         (resolved-secrets nil))
    (agent-switch-job-create
     :description (format "Activate %s/%s"
                          (agent-switch-client-id client)
                          (agent-switch-profile-id profile))
     :canceler (lambda ()
               (setq cancelled t)
               (when (agent-switch-job-p active-child)
                 (agent-switch-job-cancel active-child)))
     :starter
     (lambda (resolve reject)
       (let (snapshot resolved-profile)
         (cl-labels
             ((reject-safe
               (stage error-value)
               (let ((agent-switch--secret-values resolved-secrets))
                 (funcall reject
                          (list 'agent-switch-error
                                (format "%s failed during %s: %s"
                                        (agent-switch-adapter-name adapter)
                                        stage
                                        (agent-switch--safe-error-message
                                         error-value))))))
              (settle
               (_stage thunk success failure)
               (if cancelled
                   (funcall failure '(quit "Activation cancelled"))
                 (condition-case error-value
                     (let ((result (funcall thunk)))
                       (if (agent-switch-job-p result)
                           (progn
                             (setq active-child result)
                             (agent-switch-job-start result success failure))
                         (funcall success result)))
                   (error (funcall failure error-value)))))
              (roll-back
               (stage error-value)
               (if (and snapshot rollback)
                   (settle "rollback"
                           (lambda ()
                             (funcall rollback client snapshot context))
                           (lambda (_value) (reject-safe stage error-value))
                           (lambda (rollback-error)
                             (funcall reject
                                      (list 'agent-switch-error
                                            (format
                                             "%s failed during %s; rollback also failed: %s"
                                             (agent-switch-adapter-name adapter)
                                             stage
                                             (let ((agent-switch--secret-values
                                                    resolved-secrets))
                                               (agent-switch--safe-error-message
                                                rollback-error)))))))
                 (reject-safe stage error-value)))
              (commit
               ()
               (condition-case error-value
                   (progn
                     (agent-switch-state-set-last-selected
                      (agent-switch-client-id client)
                      (agent-switch-profile-id profile)
                      profile)
                     (funcall resolve profile))
                 (error (roll-back "state commit" error-value))))
              (verify
               ()
               (settle "verification"
                       (lambda () (funcall current-fn client context))
                       (lambda (current)
                         (if (agent-switch-profile-current-p
                              client resolved-profile current context)
                             (commit)
                           (roll-back
                            "verification"
                            '(agent-switch-error
                              "Client state does not match the selected profile"))))
                       (lambda (error-value)
                         (roll-back "verification" error-value))))
              (activate-profile
               ()
               (settle "activation"
                       (lambda ()
                         (funcall activate client resolved-profile context))
                       (lambda (_value) (verify))
                       (lambda (error-value)
                         (roll-back "activation" error-value))))
              (take-snapshot
               ()
               (if snapshot-fn
                   (settle "snapshot"
                           (lambda ()
                             (funcall snapshot-fn client profile context))
                           (lambda (value)
                             (setq snapshot value)
                             (setq context (plist-put context :snapshot value))
                             (activate-profile))
                           (lambda (error-value)
                             (reject-safe "snapshot" error-value)))
                 (activate-profile)))
              (resolve-secrets
               ()
               (condition-case error-value
                   (let ((result (agent-switch-resolve-profile-secrets profile)))
                     (setq resolved-profile (car result))
                     (setq resolved-secrets (cdr result))
                     (take-snapshot))
                 (error (reject-safe "secret resolution" error-value))))
              (validate-profile
               ()
               (if validate
                   (settle "validation"
                           (lambda ()
                             (funcall validate client profile context))
                           (lambda (_value) (resolve-secrets))
                           (lambda (error-value)
                             (reject-safe "validation" error-value)))
                 (resolve-secrets))))
           (validate-profile)))))))

(provide 'agent-switch-core)

;;; agent-switch-core.el ends here
