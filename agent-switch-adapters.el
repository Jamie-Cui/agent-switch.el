;;; agent-switch-adapters.el --- Built-in LLM client adapters -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Built-in Claude Code, Codex, gptel Default, and OpenCode Global Adapters.

;;; Code:

(require 'cl-lib)
(require 'diff)
(require 'subr-x)
(require 'agent-switch-core)
(require 'agent-switch-storage)

(declare-function gptel-backend-models "gptel-request")
(declare-function gptel-backend-name "gptel-request")
(declare-function gptel-get-backend "gptel-request")
(declare-function tomelr-encode "tomelr")
(declare-function toml:read-from-string "toml")

(defcustom agent-switch-claude-config-directory
  (expand-file-name "~/.claude/")
  "Claude Code configuration directory."
  :type 'directory
  :group 'agent-switch)

(defcustom agent-switch-codex-home
  (expand-file-name "~/.codex/")
  "Codex home directory."
  :type 'directory
  :group 'agent-switch)

(defcustom agent-switch-opencode-config-file nil
  "OpenCode global configuration file.
When nil, prefer opencode.jsonc when it exists, otherwise use
opencode.json below XDG_CONFIG_HOME or ~/.config."
  :type '(choice (const :tag "Auto" nil) file)
  :group 'agent-switch)

(defcustom agent-switch-confirm-canonical-rewrite t
  "Whether to confirm the first canonical Codex TOML rewrite per source hash."
  :type 'boolean
  :group 'agent-switch)

(defconst agent-switch--claude-owned-env-keys
  '("ANTHROPIC_API_KEY"
    "ANTHROPIC_AUTH_TOKEN"
    "ANTHROPIC_BASE_URL"
    "ANTHROPIC_MODEL"
    "ANTHROPIC_DEFAULT_HAIKU_MODEL"
    "ANTHROPIC_DEFAULT_SONNET_MODEL"
    "ANTHROPIC_DEFAULT_OPUS_MODEL"
    "ANTHROPIC_SMALL_FAST_MODEL")
  "Claude environment keys owned by the built-in Adapter.")

(defun agent-switch--claude-settings-path ()
  "Return Claude Code global settings path."
  (expand-file-name "settings.json"
                    (file-name-as-directory
                     (expand-file-name agent-switch-claude-config-directory))))

(defun agent-switch--codex-config-path ()
  "Return Codex global configuration path."
  (expand-file-name "config.toml"
                    (file-name-as-directory
                     (expand-file-name agent-switch-codex-home))))

(defun agent-switch--opencode-default-directory ()
  "Return OpenCode global configuration directory."
  (expand-file-name
   "opencode/"
   (file-name-as-directory
    (or (getenv "XDG_CONFIG_HOME") (expand-file-name "~/.config/")))))

(defun agent-switch--opencode-config-path ()
  "Return effective OpenCode global configuration path."
  (if agent-switch-opencode-config-file
      (expand-file-name agent-switch-opencode-config-file)
    (let* ((directory (agent-switch--opencode-default-directory))
           (jsonc (expand-file-name "opencode.jsonc" directory)))
      (if (file-exists-p jsonc)
          jsonc
        (expand-file-name "opencode.json" directory)))))

(defun agent-switch--read-json-file (path)
  "Read JSON object from PATH, returning an empty object when absent."
  (if (file-exists-p path)
      (let ((value (agent-switch-parse-json
                    (agent-switch--read-file-text path)
                    (file-name-nondirectory path))))
        (unless (hash-table-p value)
          (signal 'agent-switch-validation-error
                  (list (format "%s must contain a JSON object"
                                (file-name-nondirectory path)))))
        value)
    (make-hash-table :test #'equal)))

(defun agent-switch--context-file-state (context path)
  "Return file state for PATH from activation CONTEXT."
  (let ((snapshot (plist-get context :snapshot)))
    (or (cl-find path snapshot :key #'agent-switch-file-state-path
                 :test #'equal)
        (agent-switch-capture-file path))))

(defun agent-switch--write-live-text (path text context)
  "Back up and atomically write TEXT to PATH using CONTEXT snapshot."
  (let ((state (agent-switch--context-file-state context path)))
    (agent-switch-backup-file path)
    (setf (agent-switch-file-state-hash state)
          (agent-switch-write-text-atomic
           path text (agent-switch-file-state-hash state) t))))

(defun agent-switch--write-live-json (path object context)
  "Back up and atomically write JSON OBJECT to PATH using CONTEXT."
  (agent-switch--write-live-text
   path (agent-switch-json-serialize object) context))

(defun agent-switch--rollback-files (_client snapshot _context)
  "Restore file SNAPSHOT for a built-in Adapter."
  (dolist (state snapshot)
    (agent-switch-restore-file state))
  t)

(defun agent-switch--secret-marker (value)
  "Return a non-reversible marker for secret VALUE."
  (let ((marker (make-hash-table :test #'equal)))
    (puthash "$secret_hash" (secure-hash 'sha256 value) marker)
    marker))

(defun agent-switch--secret-marker-p (value)
  "Return non-nil when VALUE is a secret marker."
  (and (hash-table-p value) (stringp (gethash "$secret_hash" value))))

(defun agent-switch--sensitive-key-p (key)
  "Return non-nil when KEY conventionally carries a secret."
  (and (stringp key)
       (string-match-p agent-switch--sensitive-key-regexp (downcase key))))

(defun agent-switch--redact-json-secrets (value &optional parent-key)
  "Copy JSON VALUE while replacing secrets below PARENT-KEY with hashes."
  (cond
   ((and parent-key
         (agent-switch--sensitive-key-p parent-key)
         (stringp value))
    (agent-switch--secret-marker value))
   ((hash-table-p value)
    (let ((copy (make-hash-table :test #'equal)))
      (maphash (lambda (key child)
                 (puthash key (agent-switch--redact-json-secrets child key) copy))
               value)
      copy))
   ((vectorp value)
    (vconcat (mapcar (lambda (child)
                       (agent-switch--redact-json-secrets child parent-key))
                     (append value nil))))
   ((consp value)
    (mapcar (lambda (child)
              (agent-switch--redact-json-secrets child parent-key))
            value))
   (t value)))

(defun agent-switch--json-subset-p (expected actual)
  "Return non-nil when JSON EXPECTED is represented by ACTUAL.
Secret references match any configured secret; resolved strings match hashed
secret markers exactly."
  (cond
   ((agent-switch-secret-reference-p expected)
    (agent-switch--secret-marker-p actual))
   ((agent-switch--secret-marker-p actual)
    (and (stringp expected)
         (equal (gethash "$secret_hash" actual)
                (secure-hash 'sha256 expected))))
   ((hash-table-p expected)
    (and (hash-table-p actual)
         (let ((matches t))
           (maphash (lambda (key value)
                      (let ((missing (make-symbol "missing")))
                        (let ((actual-value (gethash key actual missing)))
                          (unless (and (not (eq actual-value missing))
                                       (agent-switch--json-subset-p
                                        value actual-value))
                            (setq matches nil)))))
                    expected)
           matches)))
   ((vectorp expected)
    (and (vectorp actual)
         (= (length expected) (length actual))
         (cl-loop for index below (length expected)
                  always (agent-switch--json-subset-p
                          (aref expected index) (aref actual index)))))
   (t (equal expected actual))))

;;; Claude Code

(defun agent-switch--claude-owned-state (settings)
  "Extract secret-safe owned state from Claude SETTINGS.
Return nil when no ANTHROPIC_* keys are configured."
  (let ((owned-env (make-hash-table :test #'equal))
        (env (gethash "env" settings)))
    (when (hash-table-p env)
      (dolist (key agent-switch--claude-owned-env-keys)
        (let ((missing (make-symbol "missing")))
          (let ((value (gethash key env missing)))
            (unless (eq value missing)
              (puthash key
                       (if (agent-switch--sensitive-key-p key)
                           (if (stringp value)
                               (agent-switch--secret-marker value)
                             value)
                         value)
                       owned-env))))))
    (if (> (hash-table-count owned-env) 0)
        (let ((owned (make-hash-table :test #'equal)))
          (puthash "env" owned-env owned)
          owned)
      nil)))

(defun agent-switch--claude-current (_client _context)
  "Return current Claude provider-owned state."
  (agent-switch--claude-owned-state
   (agent-switch--read-json-file (agent-switch--claude-settings-path))))

(defun agent-switch--claude-validate (_client profile _context)
  "Validate Claude PROFILE."
  (let* ((payload (agent-switch-profile-payload profile))
         (env (gethash "env" payload)))
    (unless (hash-table-p env)
      (signal 'agent-switch-validation-error
              '("Claude payload requires an env object")))
    (maphash
     (lambda (key _value)
       (unless (member key agent-switch--claude-owned-env-keys)
         (signal 'agent-switch-validation-error
                 (list (format "Claude env key is not provider-owned: %s" key)))))
     env)
    t))

(defun agent-switch--claude-snapshot (_client _profile _context)
  "Snapshot Claude settings for rollback."
  (list (agent-switch-capture-file (agent-switch--claude-settings-path))))

(defun agent-switch--claude-activate (_client profile context)
  "Activate resolved Claude PROFILE using CONTEXT."
  (let* ((path (agent-switch--claude-settings-path))
         (settings (agent-switch--read-json-file path))
         (env (or (gethash "env" settings)
                  (let ((new (make-hash-table :test #'equal)))
                    (puthash "env" new settings)
                    new)))
         (profile-env (gethash "env" (agent-switch-profile-payload profile))))
    (unless (hash-table-p env)
      (setq env (make-hash-table :test #'equal))
      (puthash "env" env settings))
    (dolist (key agent-switch--claude-owned-env-keys)
      (remhash key env))
    (maphash (lambda (key value) (puthash key value env)) profile-env)
    (agent-switch--write-live-json path settings context)
    t))

(defun agent-switch--claude-profile-current-p (_client profile current _context)
  "Return non-nil when Claude PROFILE matches CURRENT state."
  (agent-switch--json-subset-p (agent-switch-profile-payload profile) current))

(defun agent-switch--claude-watch-paths (_client _context)
  "Return paths watched for Claude changes."
  (list (agent-switch--claude-settings-path)))

;;; TOML helpers and Codex

(defun agent-switch--ensure-toml ()
  "Load structural TOML dependencies or signal."
  (unless (and (require 'toml nil t) (require 'tomelr nil t))
    (signal 'agent-switch-error
            '("Codex support requires the toml and tomelr packages"))))

(defun agent-switch--alist-get (key alist &optional default)
  "Return string KEY from ALIST or DEFAULT."
  (let ((cell (assoc-string key alist t)))
    (if cell (cdr cell) default)))

(defun agent-switch--alist-set (key value alist)
  "Set string KEY to VALUE in ALIST and return ALIST."
  (let ((cell (assoc-string key alist t)))
    (if cell
        (setcdr cell value)
      (setq alist (cons (cons key value) alist)))
    alist))

(defun agent-switch--alist-delete (key alist)
  "Delete string KEY from ALIST."
  (cl-remove key alist :key #'car :test #'string-equal))

(defun agent-switch--toml-table-p (value)
  "Return non-nil when VALUE is a TOML table alist."
  (and (listp value)
       value
       (cl-every (lambda (entry)
                   (and (consp entry) (stringp (car entry))))
                 value)))

(defun agent-switch--toml-order (value)
  "Recursively order TOML VALUE with scalars before tables."
  (cond
   ((agent-switch--toml-table-p value)
    (let (scalars tables)
      (dolist (entry value)
        (let ((converted (cons (car entry)
                               (agent-switch--toml-order (cdr entry)))))
          (if (or (agent-switch--toml-table-p (cdr entry))
                  (and (vectorp (cdr entry))
                       (> (length (cdr entry)) 0)
                       (agent-switch--toml-table-p (aref (cdr entry) 0))))
              (push converted tables)
            (push converted scalars))))
      (append (nreverse scalars) (nreverse tables))))
   ((vectorp value)
    (vconcat (mapcar #'agent-switch--toml-order (append value nil))))
   (t value)))

(defun agent-switch--json-to-toml (value)
  "Convert JSON VALUE to tomelr-compatible data."
  (cond
   ((hash-table-p value)
    (let (alist)
      (maphash (lambda (key child)
                 (push (cons key (agent-switch--json-to-toml child)) alist))
               value)
      (nreverse alist)))
   ((vectorp value)
    (vconcat (mapcar #'agent-switch--json-to-toml (append value nil))))
   ((eq value agent-switch-json-false) :false)
   ((eq value agent-switch-json-null) nil)
   (t value)))

(defun agent-switch--toml-to-json (value)
  "Convert TOML VALUE to JSON-compatible data."
  (cond
   ((agent-switch--toml-table-p value)
    (let ((object (make-hash-table :test #'equal)))
      (dolist (entry value)
        (puthash (car entry) (agent-switch--toml-to-json (cdr entry)) object))
      object))
   ((vectorp value)
    (vconcat (mapcar #'agent-switch--toml-to-json (append value nil))))
   ((eq value :false) agent-switch-json-false)
   (t value)))

(defun agent-switch--read-toml-file (path)
  "Parse PATH as TOML, returning an empty alist when absent."
  (agent-switch--ensure-toml)
  (if (file-exists-p path)
      (condition-case nil
          (toml:read-from-string (agent-switch--read-file-text path))
        (error
         (signal 'agent-switch-validation-error
                 (list (format "Invalid TOML in %s"
                               (file-name-nondirectory path))))))
    nil))

(defun agent-switch--encode-toml (data)
  "Encode TOML DATA and verify that it reparses."
  (agent-switch--ensure-toml)
  (let* ((ordered (agent-switch--toml-order data))
         (text (tomelr-encode ordered)))
    (unless (string-suffix-p "\n" text)
      (setq text (concat text "\n")))
    (condition-case nil
        (toml:read-from-string text)
      (error
       (signal 'agent-switch-error
               '("Generated Codex TOML failed verification"))))
    text))

(defun agent-switch--codex-provider-state (config provider-id)
  "Return provider table for PROVIDER-ID from TOML CONFIG."
  (let ((providers (agent-switch--alist-get "model_providers" config)))
    (if (agent-switch--toml-table-p providers)
        (or (agent-switch--alist-get provider-id providers) nil)
      nil)))

(defun agent-switch--codex-current (_client _context)
  "Return current Codex provider-owned state.
Return nil when no provider is configured."
  (let* ((config (agent-switch--read-toml-file
                  (agent-switch--codex-config-path)))
         (provider-id (agent-switch--alist-get "model_provider" config)))
    (when provider-id
      (let ((payload (make-hash-table :test #'equal)))
        (puthash "provider-id" provider-id payload)
        (when-let* ((model (agent-switch--alist-get "model" config)))
          (puthash "model" model payload))
        (when-let* ((small (agent-switch--alist-get "small_model" config)))
          (puthash "small-model" small payload))
        (let ((provider-state
               (agent-switch--codex-provider-state config provider-id)))
          (puthash "provider"
                   (if provider-state
                       (agent-switch--redact-json-secrets
                        (agent-switch--toml-to-json provider-state))
                     (make-hash-table :test #'equal))
                   payload))
        payload))))

(defun agent-switch--codex-validate (_client profile _context)
  "Validate Codex PROFILE."
  (let* ((payload (agent-switch-profile-payload profile))
         (provider-id (gethash "provider-id" payload))
         (model (gethash "model" payload))
         (provider (gethash "provider" payload)))
    (unless (and (stringp provider-id) (not (string-empty-p provider-id)))
      (signal 'agent-switch-validation-error
              '("Codex provider-id is required")))
    (unless (and (stringp model) (not (string-empty-p model)))
      (signal 'agent-switch-validation-error '("Codex model is required")))
    (unless (or (null provider) (hash-table-p provider))
      (signal 'agent-switch-validation-error
              '("Codex provider patch must be an object")))
    t))

(defun agent-switch--codex-snapshot (_client _profile _context)
  "Snapshot Codex config for rollback."
  (list (agent-switch-capture-file (agent-switch--codex-config-path))))

(defun agent-switch--show-rewrite-diff (path old-text new-text)
  "Display a unified diff for PATH from OLD-TEXT to NEW-TEXT."
  (let ((old-file (make-temp-file "agent-switch-old-"))
        (new-file (make-temp-file "agent-switch-new-")))
    (unwind-protect
        (progn
          (with-temp-file old-file (insert old-text))
          (with-temp-file new-file (insert new-text))
          (let ((buffer (diff-no-select old-file new-file "-u" t)))
            (with-current-buffer buffer
              (rename-buffer "*agent-switch canonical rewrite*" t))
            (display-buffer buffer)))
      (ignore-errors (delete-file old-file))
      (ignore-errors (delete-file new-file))))
  (message "Canonical rewrite preview for %s" (abbreviate-file-name path)))

(defun agent-switch--confirm-codex-rewrite (path old-text new-text context)
  "Confirm canonical rewrite of PATH from OLD-TEXT to NEW-TEXT.
CONTEXT determines whether an interactive confirmation is available."
  (when (and agent-switch-confirm-canonical-rewrite
             (file-exists-p path)
             (not (equal old-text new-text)))
    (let ((hash (agent-switch-content-hash old-text))
          (key "codex-config"))
      (unless (agent-switch-state-canonical-confirmed-p key hash)
        (unless (plist-get context :interactive)
          (signal 'agent-switch-error
                  '("Codex canonical rewrite requires interactive confirmation")))
        (agent-switch--show-rewrite-diff path old-text new-text)
        (unless (yes-or-no-p
                 "Rewrite Codex config.toml and lose comments/order? ")
          (user-error "Cancelled"))
        (agent-switch-state-confirm-canonical key hash)))))

(defun agent-switch--codex-activate (_client profile context)
  "Activate resolved Codex PROFILE using CONTEXT."
  (let* ((path (agent-switch--codex-config-path))
         (old-text (if (file-exists-p path)
                       (agent-switch--read-file-text path) ""))
         (config (agent-switch--read-toml-file path))
         (payload (agent-switch-profile-payload profile))
         (provider-id (gethash "provider-id" payload))
         (model (gethash "model" payload))
         (small-model (gethash "small-model" payload))
         (patch (or (gethash "provider" payload)
                    (make-hash-table :test #'equal)))
         (providers (or (agent-switch--alist-get "model_providers" config) nil))
         (existing (or (and (agent-switch--toml-table-p providers)
                            (agent-switch--alist-get provider-id providers))
                       nil))
         (merged (agent-switch-json-deep-merge
                  (agent-switch--toml-to-json existing) patch)))
    (setq config (agent-switch--alist-set "model_provider" provider-id config))
    (setq config (agent-switch--alist-set "model" model config))
    (setq config (if (and (stringp small-model)
                          (not (string-empty-p small-model)))
                     (agent-switch--alist-set "small_model" small-model config)
                   (agent-switch--alist-delete "small_model" config)))
    (unless (agent-switch--toml-table-p providers)
      (setq providers nil))
    (setq providers
          (agent-switch--alist-set provider-id
                                   (agent-switch--json-to-toml merged)
                                   providers))
    (setq config (agent-switch--alist-set "model_providers" providers config))
    (let ((new-text (agent-switch--encode-toml config)))
      (agent-switch--confirm-codex-rewrite
       path old-text new-text context)
      (agent-switch--write-live-text path new-text context))
    t))

(defun agent-switch--codex-profile-current-p (_client profile current _context)
  "Return non-nil when Codex PROFILE matches CURRENT."
  (agent-switch--json-subset-p (agent-switch-profile-payload profile) current))

(defun agent-switch--codex-watch-paths (_client _context)
  "Return paths watched for Codex changes."
  (list (agent-switch--codex-config-path)))

;;; gptel defaults

(defun agent-switch--ensure-gptel ()
  "Load gptel or signal a clear error."
  (unless (require 'gptel nil t)
    (signal 'agent-switch-error '("gptel is not installed"))))

(defun agent-switch--gptel-backend-name (backend)
  "Return stable string name for gptel BACKEND."
  (and backend (gptel-backend-name backend)))

(defun agent-switch--gptel-current (_client _context)
  "Return gptel global default backend and model.
Return nil when no backend is configured."
  (agent-switch--ensure-gptel)
  (let ((backend (default-toplevel-value 'gptel-backend))
        (model (default-toplevel-value 'gptel-model)))
    (when backend
      (let ((payload (make-hash-table :test #'equal)))
        (puthash "backend-name" (agent-switch--gptel-backend-name backend) payload)
        (when model
          (puthash "model" (if (symbolp model) (symbol-name model) model) payload))
        payload))))

(defun agent-switch--gptel-models-for-backend (backend-name)
  "Return model name strings for gptel BACKEND-NAME."
  (agent-switch--ensure-gptel)
  (mapcar (lambda (model)
            (if (symbolp model) (symbol-name model) (format "%s" model)))
          (gptel-backend-models (gptel-get-backend backend-name))))

(defun agent-switch--gptel-validate (_client profile _context)
  "Validate gptel PROFILE references."
  (let* ((payload (agent-switch-profile-payload profile))
         (backend-name (gethash "backend-name" payload))
         (model (gethash "model" payload)))
    (unless (and (stringp backend-name) (not (string-empty-p backend-name)))
      (signal 'agent-switch-validation-error '("gptel backend-name is required")))
    (unless (and (stringp model) (not (string-empty-p model)))
      (signal 'agent-switch-validation-error '("gptel model is required")))
    (unless (member model (agent-switch--gptel-models-for-backend backend-name))
      (signal 'agent-switch-validation-error
              (list (format "Model %s is not registered for backend %s"
                            model backend-name))))
    t))

(defun agent-switch--gptel-snapshot (_client _profile _context)
  "Snapshot gptel global defaults."
  (agent-switch--ensure-gptel)
  (list (default-toplevel-value 'gptel-backend)
        (default-toplevel-value 'gptel-model)))

(defun agent-switch--gptel-activate (_client profile _context)
  "Activate gptel PROFILE as global defaults."
  (agent-switch--ensure-gptel)
  (let* ((payload (agent-switch-profile-payload profile))
         (backend-name (gethash "backend-name" payload))
         (model (gethash "model" payload)))
    (set-default-toplevel-value 'gptel-backend
                                (gptel-get-backend backend-name))
    (set-default-toplevel-value 'gptel-model (intern model))
    t))

(defun agent-switch--gptel-rollback (_client snapshot _context)
  "Restore gptel defaults from SNAPSHOT."
  (set-default-toplevel-value 'gptel-backend (nth 0 snapshot))
  (set-default-toplevel-value 'gptel-model (nth 1 snapshot))
  t)

(defun agent-switch--gptel-profile-current-p (_client profile current _context)
  "Return non-nil when gptel PROFILE matches CURRENT defaults."
  (agent-switch--json-subset-p (agent-switch-profile-payload profile) current))

(defun agent-switch--gptel-watch-setup (_client callback)
  "Watch gptel default variables and invoke CALLBACK after changes.
Return a cleanup function removing both variable watchers."
  (agent-switch--ensure-gptel)
  (let ((watcher (lambda (_symbol _new-value operation _where)
                   (when (memq operation '(set let unlet makunbound))
                     (funcall callback)))))
    (add-variable-watcher 'gptel-backend watcher)
    (add-variable-watcher 'gptel-model watcher)
    (lambda ()
      (remove-variable-watcher 'gptel-backend watcher)
      (remove-variable-watcher 'gptel-model watcher))))

;;; OpenCode global JSON/JSONC

(defun agent-switch--jsonc-clean (text)
  "Return JSONC TEXT with comments and trailing commas removed.
String contents and line positions are preserved."
  (let ((length (length text))
        (index 0)
        (state 'normal)
        (output (get-buffer-create " *agent-switch-jsonc*")))
    (unwind-protect
        (with-current-buffer output
          (erase-buffer)
          (while (< index length)
            (let* ((char (aref text index))
                   (next (and (< (1+ index) length)
                              (aref text (1+ index)))))
              (pcase state
                ('string
                 (insert-char char)
                 (cond ((eq char ?\\) (setq state 'escape))
                       ((eq char ?\") (setq state 'normal))))
                ('escape
                 (insert-char char)
                 (setq state 'string))
                ('line-comment
                 (if (eq char ?\n)
                     (progn (insert-char char) (setq state 'normal))
                   (insert-char ?\s)))
                ('block-comment
                 (cond
                  ((and (eq char ?*) (eq next ?/))
                   (insert "  ")
                   (setq index (1+ index) state 'normal))
                  ((eq char ?\n) (insert-char char))
                  (t (insert-char ?\s))))
                (_
                 (cond
                  ((eq char ?\") (insert-char char) (setq state 'string))
                  ((and (eq char ?/) (eq next ?/))
                   (insert "  ")
                   (setq index (1+ index) state 'line-comment))
                  ((and (eq char ?/) (eq next ?*))
                   (insert "  ")
                   (setq index (1+ index) state 'block-comment))
                  (t (insert-char char))))))
            (setq index (1+ index)))
          (let ((without-comments (buffer-string)))
            (erase-buffer)
            (setq index 0 state 'normal length (length without-comments))
            (while (< index length)
              (let ((char (aref without-comments index)))
                (pcase state
                  ('string
                   (insert-char char)
                   (cond ((eq char ?\\) (setq state 'escape))
                         ((eq char ?\") (setq state 'normal))))
                  ('escape (insert-char char) (setq state 'string))
                  (_
                   (cond
                    ((eq char ?\") (insert-char char) (setq state 'string))
                    ((eq char ?,)
                     (let ((lookahead (1+ index)))
                       (while (and (< lookahead length)
                                   (memq (aref without-comments lookahead)
                                         '(?\s ?\t ?\r ?\n)))
                         (setq lookahead (1+ lookahead)))
                       (unless (and (< lookahead length)
                                    (memq (aref without-comments lookahead)
                                          '(?} ?\])))
                         (insert-char char))))
                    (t (insert-char char)))))
              (setq index (1+ index))))
            (buffer-string)))
      (kill-buffer output))))

(defun agent-switch--read-opencode-file (path)
  "Read OpenCode JSON or JSONC object from PATH."
  (if (not (file-exists-p path))
      (make-hash-table :test #'equal)
    (let ((value (agent-switch-parse-json
                  (agent-switch--jsonc-clean
                   (agent-switch--read-file-text path))
                  (file-name-nondirectory path))))
      (unless (hash-table-p value)
        (signal 'agent-switch-validation-error
                '("OpenCode global config must be a JSON object")))
      value)))

(defun agent-switch--model-provider-id (model)
  "Return provider prefix from OpenCode MODEL."
  (and (stringp model)
       (string-match "\\`\\([^/]+\\)/" model)
       (match-string 1 model)))

(defun agent-switch--opencode-current (_client _context)
  "Return current OpenCode global provider-owned state.
Return nil when no model is configured."
  (let* ((config (agent-switch--read-opencode-file
                  (agent-switch--opencode-config-path)))
         (model (gethash "model" config))
         (provider-id (agent-switch--model-provider-id model)))
    (when model
      (let ((payload (make-hash-table :test #'equal))
            (providers (gethash "provider" config)))
        (when provider-id (puthash "provider-id" provider-id payload))
        (puthash "model" model payload)
        (when-let* ((small (gethash "small_model" config)))
          (puthash "small-model" small payload))
        (when (and provider-id (hash-table-p providers))
          (puthash "provider"
                   (agent-switch--redact-json-secrets
                    (or (gethash provider-id providers)
                        (make-hash-table :test #'equal)))
                   payload))
        payload))))

(defun agent-switch--opencode-validate (_client profile _context)
  "Validate OpenCode PROFILE."
  (let* ((payload (agent-switch-profile-payload profile))
         (provider-id (gethash "provider-id" payload))
         (model (gethash "model" payload))
         (provider (gethash "provider" payload)))
    (unless (and (stringp provider-id) (not (string-empty-p provider-id)))
      (signal 'agent-switch-validation-error
              '("OpenCode provider-id is required")))
    (unless (and (stringp model)
                 (equal (agent-switch--model-provider-id model) provider-id))
      (signal 'agent-switch-validation-error
              '("OpenCode model must use provider-id/model-id form")))
    (unless (or (null provider) (hash-table-p provider))
      (signal 'agent-switch-validation-error
              '("OpenCode provider patch must be an object")))
    t))

(defun agent-switch--opencode-snapshot (_client _profile _context)
  "Snapshot OpenCode global config for rollback."
  (list (agent-switch-capture-file (agent-switch--opencode-config-path))))

(defun agent-switch--opencode-activate (_client profile context)
  "Activate resolved OpenCode PROFILE globally using CONTEXT."
  (let* ((path (agent-switch--opencode-config-path))
         (config (agent-switch--read-opencode-file path))
         (payload (agent-switch-profile-payload profile))
         (provider-id (gethash "provider-id" payload))
         (model (gethash "model" payload))
         (small-model (gethash "small-model" payload))
         (patch (or (gethash "provider" payload)
                    (make-hash-table :test #'equal)))
         (providers (or (gethash "provider" config)
                        (let ((new (make-hash-table :test #'equal)))
                          (puthash "provider" new config)
                          new)))
         (existing (or (gethash provider-id providers)
                       (make-hash-table :test #'equal))))
    (unless (hash-table-p providers)
      (setq providers (make-hash-table :test #'equal))
      (puthash "provider" providers config))
    (puthash provider-id (agent-switch-json-deep-merge existing patch) providers)
    (puthash "model" model config)
    (if (and (stringp small-model) (not (string-empty-p small-model)))
        (puthash "small_model" small-model config)
      (remhash "small_model" config))
    (agent-switch--write-live-json path config context)
    t))

(defun agent-switch--opencode-profile-current-p (_client profile current _context)
  "Return non-nil when OpenCode PROFILE matches CURRENT."
  (agent-switch--json-subset-p (agent-switch-profile-payload profile) current))

(defun agent-switch--opencode-watch-paths (_client _context)
  "Return paths watched for OpenCode global changes."
  (list (agent-switch--opencode-config-path)))

;;; Registration

(defun agent-switch--template-object (&rest entries)
  "Return a JSON object populated from ENTRIES cons cells."
  (let ((object (make-hash-table :test #'equal)))
    (dolist (entry entries object)
      (puthash (car entry) (cdr entry) object))))

(defun agent-switch--claude-profile-template (_client _context)
  "Return a new Claude Profile payload template."
  (agent-switch--template-object
   (cons "env" (agent-switch--template-object
                '("ANTHROPIC_BASE_URL" . "")
                '("ANTHROPIC_MODEL" . "")
                '("ANTHROPIC_DEFAULT_HAIKU_MODEL" . "")
                '("ANTHROPIC_DEFAULT_SONNET_MODEL" . "")
                '("ANTHROPIC_DEFAULT_OPUS_MODEL" . "")))))

(defun agent-switch--codex-profile-template (_client _context)
  "Return a new Codex Profile payload template."
  (agent-switch--template-object
   '("provider-id" . "") '("model" . "") '("small-model" . "")
   (cons "provider" (agent-switch--template-object
                     '("base_url" . "") '("env_key" . "")
                     '("wire_api" . "responses")))))

(defun agent-switch--gptel-profile-template (_client _context)
  "Return a new gptel Profile payload template."
  (agent-switch--template-object '("backend-name" . "") '("model" . "")))

(defun agent-switch--opencode-profile-template (_client _context)
  "Return a new OpenCode Profile payload template."
  (agent-switch--template-object
   '("provider-id" . "") '("model" . "") '("small-model" . "")
   (cons "provider" (agent-switch--template-object
                     '("npm" . "")
                     (cons "options" (agent-switch--template-object
                                      '("baseURL" . "")))))))

(defun agent-switch-register-builtins ()
  "Register built-in adapters and clients."
  (agent-switch-define-adapter claude
    :name "Claude Code"
    :current #'agent-switch--claude-current
    :activate #'agent-switch--claude-activate
    :validate #'agent-switch--claude-validate
    :snapshot #'agent-switch--claude-snapshot
    :rollback #'agent-switch--rollback-files
    :profile-current-p #'agent-switch--claude-profile-current-p
    :watch-paths #'agent-switch--claude-watch-paths
    :profile-template #'agent-switch--claude-profile-template)
  (agent-switch-register-client 'claude :name "Claude Code" :adapter 'claude)

  (agent-switch-define-adapter codex
    :name "Codex"
    :current #'agent-switch--codex-current
    :activate #'agent-switch--codex-activate
    :validate #'agent-switch--codex-validate
    :snapshot #'agent-switch--codex-snapshot
    :rollback #'agent-switch--rollback-files
    :profile-current-p #'agent-switch--codex-profile-current-p
    :watch-paths #'agent-switch--codex-watch-paths
    :profile-template #'agent-switch--codex-profile-template)
  (agent-switch-register-client 'codex :name "Codex" :adapter 'codex)

  (agent-switch-define-adapter gptel-default
    :name "gptel Default"
    :current #'agent-switch--gptel-current
    :activate #'agent-switch--gptel-activate
    :validate #'agent-switch--gptel-validate
    :snapshot #'agent-switch--gptel-snapshot
    :rollback #'agent-switch--gptel-rollback
    :profile-current-p #'agent-switch--gptel-profile-current-p
    :watch-setup #'agent-switch--gptel-watch-setup
    :profile-template #'agent-switch--gptel-profile-template)
  (agent-switch-register-client 'gptel-default
                                :name "gptel Default"
                                :adapter 'gptel-default)

  (agent-switch-define-adapter opencode-global
    :name "OpenCode Global"
    :current #'agent-switch--opencode-current
    :activate #'agent-switch--opencode-activate
    :validate #'agent-switch--opencode-validate
    :snapshot #'agent-switch--opencode-snapshot
    :rollback #'agent-switch--rollback-files
    :profile-current-p #'agent-switch--opencode-profile-current-p
    :watch-paths #'agent-switch--opencode-watch-paths
    :profile-template #'agent-switch--opencode-profile-template)
  (agent-switch-register-client 'opencode-global
                                :name "OpenCode Global"
                                :adapter 'opencode-global))

(agent-switch-register-builtins)

(provide 'agent-switch-adapters)

;;; agent-switch-adapters.el ends here
