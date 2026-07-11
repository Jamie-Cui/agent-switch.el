;;; agent-switch-ui.el --- Section dashboard and profile editor -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Magit-like section dashboard, optional Evil integration, transient actions,
;; file watchers, semantic faces, and the managed Profile widget editor.

;;; Code:

(require 'cl-lib)
(require 'filenotify)
(require 'seq)
(require 'subr-x)
(require 'transient)
(require 'agent-switch-core)
(require 'agent-switch-storage)

(declare-function evil-define-key* "evil-core")
(declare-function evil-insert-state "evil-commands")
(declare-function evil-normal-state "evil-commands")
(declare-function evil-set-initial-state "evil-core")

(defcustom agent-switch-buffer-name "*agent-switch*"
  "Name of the agent-switch dashboard buffer."
  :type 'string
  :group 'agent-switch)

(defcustom agent-switch-watch-debounce 0.2
  "Seconds to debounce external configuration change events."
  :type 'number
  :group 'agent-switch)

(defcustom agent-switch-highlight-current-section t
  "Whether to highlight the section at point in the dashboard."
  :type 'boolean
  :group 'agent-switch)

(defcustom agent-switch-client-name-width 22
  "Display width reserved for Client names in section headings."
  :type 'integer
  :group 'agent-switch)

(defface agent-switch-title
  '((t :inherit (variable-pitch bold) :height 1.2))
  "Standalone view title face."
  :group 'agent-switch)

(defface agent-switch-section-heading
  '((t :inherit bold))
  "Section heading face."
  :group 'agent-switch)

(defface agent-switch-section-highlight
  '((t :inherit secondary-selection :extend t))
  "Face for highlighting the section at point."
  :group 'agent-switch)

(defface agent-switch-profile-summary
  '((t :inherit fixed-pitch))
  "Provider Profile summary row face."
  :group 'agent-switch)

(defface agent-switch-current
  '((t :inherit (bold success)))
  "Current Profile face."
  :group 'agent-switch)

(defface agent-switch-status-success
  '((t :inherit success))
  "Successful status face."
  :group 'agent-switch)

(defface agent-switch-status-warning
  '((t :inherit warning))
  "Warning status face."
  :group 'agent-switch)

(defface agent-switch-status-error
  '((t :inherit error))
  "Error status face."
  :group 'agent-switch)

(defface agent-switch-tag
  '((t :inherit font-lock-constant-face))
  "Profile tag face."
  :group 'agent-switch)

(defface agent-switch-key
  '((t :inherit font-lock-keyword-face))
  "Detail key face."
  :group 'agent-switch)

(defface agent-switch-secondary
  '((t :inherit (fixed-pitch shadow)))
  "IDs, paths, and secondary data face."
  :group 'agent-switch)

(cl-defstruct (agent-switch-section
               (:constructor agent-switch--make-section))
  id type parent start end value expanded-p)

(cl-defstruct (agent-switch-client-view
               (:constructor agent-switch--make-client-view))
  client profiles current current-profile last-selected error loading-p
  bootstrap-status)

(defvar-local agent-switch--sections nil)
(defvar-local agent-switch--visibility nil)
(defvar-local agent-switch--section-highlight-overlay nil)
(defvar-local agent-switch--cycle-state 1)
(defvar-local agent-switch--generation 0)
(defvar-local agent-switch--current-cache nil)
(defvar agent-switch--running-jobs (make-hash-table :test #'equal)
  "Mutating Client Jobs shared by all agent-switch UI buffers.")

;; `defvar' preserves the old default during a live upgrade from the former
;; buffer-local implementation, where that default was nil.
(unless (hash-table-p (default-value 'agent-switch--running-jobs))
  (set-default 'agent-switch--running-jobs
               (make-hash-table :test #'equal)))
(defvar-local agent-switch--watch-descriptors nil)
(defvar-local agent-switch--watch-cleanups nil)
(defvar-local agent-switch--watch-timer nil)

(defvar-local agent-switch-profile-edit--client nil)
(defvar-local agent-switch-profile-edit--profile nil)
(defvar-local agent-switch-profile-edit--new-p nil)
(defvar-local agent-switch-profile-edit--core-widgets nil)
(defvar-local agent-switch-profile-edit--widgets nil)
(defvar-local agent-switch-profile-edit--dirty-p nil)
(defvar-local agent-switch-profile-edit--saving-p nil)
(defvar-local agent-switch-profile-edit--validation-job nil)

(defun agent-switch--section-id (&rest parts)
  "Return stable section ID by joining PARTS."
  (string-join (mapcar (lambda (part) (format "%s" part)) parts) "/"))

(defun agent-switch--visibility-value (id type)
  "Return visibility for section ID of TYPE."
  (let ((missing (make-symbol "missing"))
        value)
    (setq value (gethash id agent-switch--visibility missing))
    (if (eq value missing)
        (eq type 'client)
      value)))

(defun agent-switch--set-visible (id visible)
  "Set section ID visibility to VISIBLE."
  (puthash id (and visible t) agent-switch--visibility))

(defun agent-switch--insert-section-heading
    (id type parent label &optional value face)
  "Insert a section heading and register it.
ID, TYPE, and PARENT describe the section.  LABEL is displayed, VALUE is
associated context, and FACE overrides the standard heading face."
  (let* ((expanded (agent-switch--visibility-value id type))
         (start (point))
         (section (agent-switch--make-section
                   :id id :type type :parent parent :start start
                   :value value :expanded-p expanded)))
    (let ((label-start (point)))
      (insert label)
      (add-face-text-property
       label-start (point) (or face 'agent-switch-section-heading) t))
    (insert "\n")
    (add-text-properties
     start (point)
     (list 'agent-switch-section-id id
           'agent-switch-section-type type
           'mouse-face 'highlight
           'help-echo "TAB toggles this section"))
    (puthash id section agent-switch--sections)
    section))

(defun agent-switch--finish-section (section)
  "Record end position for SECTION."
  (setf (agent-switch-section-end section) (point))
  section)

(defun agent-switch--insert-detail-line (key value &optional value-face)
  "Insert a detail line containing KEY and VALUE using VALUE-FACE."
  (insert "    " (propertize (concat key ":") 'face 'agent-switch-key)
          " " (propertize (format "%s" value)
                           'face (or value-face 'default)) "\n"))

(defun agent-switch--display-width (text width)
  "Return TEXT truncated and padded to display WIDTH."
  (let* ((plain (or text ""))
         (truncated (if (> (string-width plain) width)
                        (truncate-string-to-width plain width nil nil "...")
                      plain))
         (padding (max 0 (- width (string-width truncated)))))
    (concat truncated (make-string padding ?\s))))

(defun agent-switch--profile-status-tag (profile current-p)
  "Return status tag for PROFILE and CURRENT-P."
  (cond
   ((not (agent-switch-profile-valid-p profile))
    (propertize "invalid" 'face 'agent-switch-status-error))
   (current-p (propertize "current" 'face 'agent-switch-status-success))
   ((eq (agent-switch-profile-ownership profile) 'external)
    (propertize "external" 'face 'agent-switch-tag))
   (t (propertize "managed" 'face 'agent-switch-tag))))

(defun agent-switch--current-cache-entry (client-id)
  "Return current-state cache entry for CLIENT-ID."
  (gethash client-id agent-switch--current-cache))

(defun agent-switch--start-current-job (client job)
  "Start asynchronous current-state JOB for CLIENT."
  (let* ((buffer (current-buffer))
         (client-id (agent-switch-client-id client))
         (generation agent-switch--generation))
    (puthash client-id (list :status 'pending :job job)
             agent-switch--current-cache)
    (agent-switch-job-start
     job
     (lambda (value)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (when (= generation agent-switch--generation)
             (puthash client-id (list :status 'ready :value value)
                      agent-switch--current-cache)
             (agent-switch-refresh t)))))
     (lambda (error-value)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (when (= generation agent-switch--generation)
             (puthash client-id
                      (list :status 'error
                            :error (agent-switch--safe-error-message error-value))
                      agent-switch--current-cache)
             (agent-switch-refresh t))))))))

(defun agent-switch--client-current (client)
  "Return (VALUE ERROR LOADING) for CLIENT current state."
  (let* ((client-id (agent-switch-client-id client))
         (entry (agent-switch--current-cache-entry client-id)))
    (cond
     ((eq (plist-get entry :status) 'ready)
      (list (plist-get entry :value) nil nil))
     ((eq (plist-get entry :status) 'pending)
      (list nil nil t))
     ((eq (plist-get entry :status) 'error)
      (list nil (plist-get entry :error) nil))
     (t
      (condition-case error-value
          (let ((value (agent-switch-call client :current nil)))
            (if (agent-switch-job-p value)
                (progn
                  (agent-switch--start-current-job client value)
                  (list nil nil t))
              (puthash client-id (list :status 'ready :value value)
                       agent-switch--current-cache)
              (list value nil nil)))
        (error
         (let ((message-text (agent-switch--safe-error-message error-value)))
           (puthash client-id (list :status 'error :error message-text)
                    agent-switch--current-cache)
           (list nil message-text nil))))))))

(defun agent-switch--matching-profile (client profiles current last-selected)
  "Return a member of PROFILES matching CLIENT CURRENT.
Prefer the Profile named by LAST-SELECTED."
  (when current
    (let ((preferred (and last-selected
                          (cl-find last-selected profiles
                                   :key #'agent-switch-profile-id
                                   :test #'equal))))
      (or (and preferred
               (agent-switch-profile-valid-p preferred)
               (condition-case nil
                   (and (agent-switch-profile-current-p
                         client preferred current nil)
                        preferred)
                 (error nil)))
          (cl-find-if
           (lambda (profile)
             (and (agent-switch-profile-valid-p profile)
                  (condition-case nil
                      (agent-switch-profile-current-p
                       client profile current nil)
                    (error nil))))
           profiles)))))

(defun agent-switch--value-has-secret-marker-p (value)
  "Return non-nil when VALUE recursively contains a secret marker."
  (cond
   ((and (hash-table-p value)
         (stringp (gethash "$secret_hash" value)))
    t)
   ((hash-table-p value)
    (let (found)
      (maphash (lambda (_key child)
                 (when (agent-switch--value-has-secret-marker-p child)
                   (setq found t)))
               value)
      found))
   ((vectorp value)
    (cl-some #'agent-switch--value-has-secret-marker-p (append value nil)))
   ((consp value)
    (cl-some #'agent-switch--value-has-secret-marker-p value))
   (t nil)))

(defun agent-switch--bootstrap-default-profile (client current)
  "Adopt secret-safe CURRENT as CLIENT's managed Default Profile.
Return the new Profile, `secret-required' when capture would omit a secret,
or nil when the Adapter cannot be initialized synchronously."
  (let* ((adapter (agent-switch-get-adapter
                   (agent-switch-client-adapter-id client)))
         (capture (agent-switch-adapter-callback adapter :capture-current))
         (state (agent-switch-read-state)))
    (cond
     ((or (null current)
          (null capture)
          (agent-switch-state-record-error state))
      nil)
     ((agent-switch--value-has-secret-marker-p current)
      'secret-required)
     (t
      (let ((payload (funcall capture client current nil)))
        (unless (agent-switch-job-p payload)
          (unless (hash-table-p payload)
            (signal 'agent-switch-validation-error
                    '("capture-current must return a Profile payload object")))
          (let* ((profile (agent-switch--make-profile
                           :id "default"
                           :client-id (agent-switch-client-id client)
                           :name "Default"
                           :description "Captured from the initial live configuration."
                           :payload payload
                           :ownership 'managed
                           :valid-p t))
                 (validate (agent-switch-adapter-callback adapter :validate))
                 (validation (and validate
                                  (funcall validate client profile nil))))
            (when (agent-switch-job-p validation)
              (setq profile nil))
            (when profile
              (unless (agent-switch-profile-current-p
                       client profile current nil)
                (signal 'agent-switch-validation-error
                        '("Captured Default does not match current state")))
              (agent-switch-save-profile profile)
              (condition-case error-value
                  (progn
                    (agent-switch-state-set-last-selected
                     (agent-switch-client-id client) "default" profile)
                    profile)
                (error
                 (ignore-errors
                   (agent-switch-delete-file-optimistic
                    (agent-switch-profile-source profile)
                    (agent-switch-profile-source-hash profile)))
                 (signal (car error-value) (cdr error-value))))))))))))

(defun agent-switch--client-view (client)
  "Build an isolated dashboard view model for CLIENT."
  (let* ((client-id (agent-switch-client-id client))
         (last-selected (condition-case nil
                            (agent-switch-state-last-selected client-id)
                          (error nil)))
         (profiles nil)
         (profile-error nil)
         (bootstrap-status nil)
         (current-result (agent-switch--client-current client))
         (current (nth 0 current-result))
         (current-error (nth 1 current-result))
         (loading-p (nth 2 current-result)))
    (condition-case error-value
        (setq profiles (agent-switch-profiles client-id))
      (error (setq profile-error
                   (agent-switch--safe-error-message error-value))))
    ;; A Job may settle synchronously after the initial discovery call has
    ;; already returned nil.  Read the ready cache once before deciding that
    ;; the Client truly has no Profiles.
    (when (and (null profiles)
               (null profile-error)
               (memq (agent-switch-profile-discovery-status client-id)
                     '(ready error)))
      (condition-case error-value
          (setq profiles (agent-switch-profiles client-id))
        (error
         (setq profile-error
               (agent-switch--safe-error-message error-value)))))
    (when (and (null profiles)
               (null profile-error)
               (null current-error)
               (not loading-p)
               (not (memq (agent-switch-profile-discovery-status client-id)
                          '(pending error))))
      (condition-case error-value
          (let ((result (agent-switch--bootstrap-default-profile
                         client current)))
            (cond
             ((agent-switch-profile-p result)
              (setq profiles (list result)
                    last-selected "default"))
             ((eq result 'secret-required)
              (setq bootstrap-status 'secret-required))))
        (error
         (setq profile-error
               (agent-switch--safe-error-message error-value)))))
    (agent-switch--make-client-view
     :client client
     :profiles profiles
     :current current
     :current-profile (agent-switch--matching-profile
                       client profiles current last-selected)
     :last-selected last-selected
     :error (or current-error profile-error)
     :loading-p loading-p
     :bootstrap-status bootstrap-status)))

(defun agent-switch--client-status (view)
  "Return propertized status string for client VIEW."
  (let* ((client (agent-switch-client-view-client view))
         (client-id (agent-switch-client-id client))
         (current (agent-switch-client-view-current view))
         (current-profile (agent-switch-client-view-current-profile view))
         (last-selected (agent-switch-client-view-last-selected view))
         (last-profile
          (and last-selected
               (cl-find last-selected (agent-switch-client-view-profiles view)
                        :key #'agent-switch-profile-id :test #'equal)))
         (applied (agent-switch-state-applied-profile client-id))
         (applied-fingerprint
          (and (hash-table-p applied) (gethash "fingerprint" applied)))
         (profile-changed
          (and last-profile applied-fingerprint
               (not (equal
                     applied-fingerprint
                     (agent-switch-profile-payload-fingerprint last-profile)))))
         (live-matches-applied
          (and current last-profile (hash-table-p applied)
               (hash-table-p (gethash "payload" applied))
               (let ((snapshot (copy-agent-switch-profile last-profile)))
                 (setf (agent-switch-profile-payload snapshot)
                       (gethash "payload" applied))
                 (condition-case nil
                     (agent-switch-profile-current-p
                      client snapshot current nil)
                   (error nil))))))
    (cond
     ((agent-switch--job-running-p client-id)
      (propertize "working" 'face 'agent-switch-status-warning))
     ((agent-switch-client-view-loading-p view)
      (propertize "loading" 'face 'shadow))
     ((agent-switch-client-view-error view)
      (propertize "error" 'face 'agent-switch-status-error))
     ((eq (agent-switch-client-view-bootstrap-status view) 'secret-required)
      (propertize "default setup required" 'face 'agent-switch-status-warning))
     ((and last-profile (not (agent-switch-profile-valid-p last-profile)))
      (concat (propertize "invalid profile, " 'face 'agent-switch-status-error)
              (agent-switch-profile-name last-profile)))
     ((and current-profile
           (equal (agent-switch-profile-id current-profile) last-selected))
      (concat "current, "
              (propertize (agent-switch-profile-name current-profile)
                          'face 'agent-switch-current)))
     (current-profile
      (concat (propertize "external selection, "
                          'face 'agent-switch-status-warning)
              (agent-switch-profile-name current-profile)))
     ((and profile-changed live-matches-applied)
      (concat (propertize "apply pending, "
                          'face 'agent-switch-status-warning)
              (agent-switch-profile-name last-profile)))
     (profile-changed
      (concat (propertize "conflict, " 'face 'agent-switch-status-error)
              (agent-switch-profile-name last-profile)))
     (current
      (propertize "unmanaged live config" 'face 'agent-switch-status-warning))
     (t (propertize "not configured" 'face 'shadow)))))

(defun agent-switch--insert-status-line (key value &optional value-face)
  "Insert a top-level status line containing KEY and VALUE using VALUE-FACE."
  (insert (propertize (concat key ":") 'face 'agent-switch-key)
          " " (propertize (format "%s" value)
                           'face (or value-face 'default)) "\n"))

(defun agent-switch--insert-status ()
  "Insert the always-visible dashboard status preamble."
  (let* ((record (agent-switch-read-state))
         (state-error (agent-switch-state-record-error record)))
    (agent-switch--insert-status-line
     "Data" (abbreviate-file-name (agent-switch--directory)))
    (agent-switch--insert-status-line
     "Clients" (number-to-string (length (agent-switch-clients))))
    (agent-switch--insert-status-line
     "State" (if state-error "damaged; read-only until reset" "ok")
     (if state-error 'agent-switch-status-error
       'agent-switch-status-success))
    (when state-error
      (agent-switch--insert-status-line
       "Error" state-error 'agent-switch-status-error))))

(defun agent-switch--insert-profile-details (client profile)
  "Insert secret-safe details for CLIENT PROFILE."
  (agent-switch--insert-detail-line
   "Name" (agent-switch-profile-name profile))
  (agent-switch--insert-detail-line
   "ID" (agent-switch-profile-id profile) 'agent-switch-secondary)
  (agent-switch--insert-detail-line
   "Ownership" (symbol-name (agent-switch-profile-ownership profile)))
  (when-let* ((description (agent-switch-profile-description profile)))
    (agent-switch--insert-detail-line "Description" description))
  (if (not (agent-switch-profile-valid-p profile))
      (progn
        (agent-switch--insert-detail-line
         "Error" (agent-switch-profile-error profile)
         'agent-switch-status-error)
        (agent-switch--insert-detail-line
         "File" (abbreviate-file-name (agent-switch-profile-source profile))
         'agent-switch-secondary))
    (let* ((adapter (agent-switch-get-adapter
                     (agent-switch-client-adapter-id client)))
           (describe (agent-switch-adapter-callback adapter :describe)))
      (when describe
        (condition-case error-value
            (dolist (entry (funcall describe client profile nil))
              (agent-switch--insert-detail-line
               (car entry) (cdr entry)
               (when (string-match-p "path\|write" (downcase (car entry)))
                 'agent-switch-secondary)))
          (error
           (agent-switch--insert-detail-line
            "Details" (agent-switch--safe-error-message error-value)
            'agent-switch-status-error)))))))

(defun agent-switch--insert-profile-section (view profile)
  "Insert PROFILE section for client VIEW."
  (let* ((client (agent-switch-client-view-client view))
         (client-id (agent-switch-client-id client))
         (id (agent-switch--section-id "client" client-id
                                       "profile" (agent-switch-profile-id profile)))
         (current-p (eq profile (agent-switch-client-view-current-profile view)))
         (marker (if current-p "  *" "   "))
         (name (agent-switch--display-width
                (agent-switch-profile-name profile) 28))
         (profile-id (agent-switch--display-width
                      (agent-switch-profile-id profile) 22))
         (label (concat marker " " name " " profile-id " "
                        (agent-switch--profile-status-tag profile current-p)))
         (section (agent-switch--insert-section-heading
                   id 'profile (agent-switch--section-id "client" client-id)
                   label profile
                   (if current-p 'agent-switch-current
                     'agent-switch-profile-summary))))
    (when (agent-switch-section-expanded-p section)
      (agent-switch--insert-profile-details client profile))
    (agent-switch--finish-section section)))

(defun agent-switch--insert-client-section (view)
  "Insert a client section from VIEW."
  (let* ((client (agent-switch-client-view-client view))
         (client-id (agent-switch-client-id client))
         (id (agent-switch--section-id "client" client-id))
         (name (propertize (agent-switch-client-name client)
                           'face 'agent-switch-key))
         (label (concat name " (" (agent-switch--client-status view) ")"))
         (section (agent-switch--insert-section-heading
                   id 'client nil label client 'default)))
    (when (agent-switch-section-expanded-p section)
      (when-let* ((error-text (agent-switch-client-view-error view)))
        (agent-switch--insert-detail-line
         "Error" error-text 'agent-switch-status-error))
      (if (agent-switch-client-view-profiles view)
          (dolist (profile (agent-switch-client-view-profiles view))
            (agent-switch--insert-profile-section view profile))
        (insert (propertize "    No profiles\n" 'face 'shadow))
        (when (eq (agent-switch-client-view-bootstrap-status view)
                  'secret-required)
          (agent-switch--insert-detail-line
           "Setup" "Use New and add secret references"
           'agent-switch-status-warning))))
    (agent-switch--finish-section section)))

(defun agent-switch--section-at-point (&optional noerror)
  "Return section at point, optionally returning nil with NOERROR."
  (let ((position (point))
        section)
    (maphash
     (lambda (_id candidate)
       (let ((start (agent-switch-section-start candidate))
             (end (agent-switch-section-end candidate)))
         (when (and start end (<= start position) (< position end)
                    (or (null section)
                        (> start (agent-switch-section-start section))
                        (and (= start (agent-switch-section-start section))
                             (< end (agent-switch-section-end section)))))
           (setq section candidate))))
     agent-switch--sections)
    (or section
        (unless noerror (user-error "No section at point")))))

(defun agent-switch--clear-section-highlight ()
  "Remove the current section highlight overlay."
  (when (overlayp agent-switch--section-highlight-overlay)
    (delete-overlay agent-switch--section-highlight-overlay))
  (setq agent-switch--section-highlight-overlay nil))

(defun agent-switch--update-section-highlight ()
  "Highlight the innermost dashboard section containing point."
  (let ((section (and agent-switch-highlight-current-section
                      (not (region-active-p))
                      (agent-switch--section-at-point t))))
    (if (not section)
        (agent-switch--clear-section-highlight)
      (unless (overlayp agent-switch--section-highlight-overlay)
        (setq agent-switch--section-highlight-overlay
              (make-overlay (point-min) (point-min) nil t))
        (overlay-put agent-switch--section-highlight-overlay
                     'font-lock-face 'agent-switch-section-highlight)
        (overlay-put agent-switch--section-highlight-overlay 'evaporate t))
      (move-overlay agent-switch--section-highlight-overlay
                    (agent-switch-section-start section)
                    (agent-switch-section-end section)
                    (current-buffer)))))

(defun agent-switch--point-section-id ()
  "Return section ID at point, or nil."
  (when-let* ((section (agent-switch--section-at-point t)))
    (agent-switch-section-id section)))

(defun agent-switch--restore-position (section-id line window-start)
  "Restore point using SECTION-ID or LINE and WINDOW-START."
  (cond
   ((and section-id (gethash section-id agent-switch--sections))
    (goto-char (agent-switch-section-start
                (gethash section-id agent-switch--sections))))
   (t
    (goto-char (point-min))
    (forward-line (max 0 (1- line)))))
  (when (and window-start (get-buffer-window (current-buffer)))
    (set-window-start (get-buffer-window (current-buffer))
                      (min window-start (point-max)) t)))

(defun agent-switch-refresh (&optional keep-cache)
  "Refresh the dashboard.
When KEEP-CACHE is non-nil, keep asynchronous current-state cache."
  (interactive)
  (unless (derived-mode-p 'agent-switch-mode)
    (user-error "Not in an agent-switch dashboard"))
  (let ((section-id (agent-switch--point-section-id))
        (line (line-number-at-pos))
        (window-start (when-let* ((window (get-buffer-window (current-buffer))))
                        (window-start window)))
        (inhibit-read-only t))
    (when (called-interactively-p 'interactive)
      (agent-switch-invalidate-discovery))
    (unless keep-cache
      (setq agent-switch--generation (1+ agent-switch--generation))
      (clrhash agent-switch--current-cache))
    (erase-buffer)
    (setq agent-switch--sections (make-hash-table :test #'equal))
    (condition-case error-value
        (progn
          (agent-switch--insert-status)
          (insert "\n")
          (dolist (client (agent-switch-clients))
            (condition-case client-error
                (agent-switch--insert-client-section
                 (agent-switch--client-view client))
              (error
               (let ((view (agent-switch--make-client-view
                            :client client
                            :profiles nil
                            :error (agent-switch--safe-error-message
                                    client-error))))
                 (agent-switch--insert-client-section view))))))
      (error
       (let ((message-text (agent-switch--safe-error-message error-value)))
         (message "agent-switch: %s" message-text)
         (insert (propertize message-text 'face 'agent-switch-status-error)
                 "\n"))))
    (agent-switch--restore-position section-id line window-start)
    (agent-switch--update-section-highlight)))

(defun agent-switch-refresh-dashboards ()
  "Refresh all open agent-switch dashboards."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'agent-switch-mode)
        (agent-switch-refresh)))))

(defun agent-switch--async-data-changed (_client-id)
  "Refresh dashboards after asynchronous Adapter discovery completes."
  (agent-switch-refresh-dashboards))

(add-hook 'agent-switch-data-changed-hook #'agent-switch--async-data-changed)

(defun agent-switch-toggle-section ()
  "Toggle the section at point."
  (interactive)
  (let ((section (agent-switch--section-at-point)))
    (agent-switch--set-visible
     (agent-switch-section-id section)
     (not (agent-switch-section-expanded-p section)))
    (agent-switch-refresh t)))

(defun agent-switch-cycle-sections ()
  "Cycle all sections through collapsed, default, and expanded states."
  (interactive)
  (setq agent-switch--cycle-state (% (1+ agent-switch--cycle-state) 3))
  (maphash
   (lambda (id section)
      (agent-switch--set-visible
       id
       (pcase agent-switch--cycle-state
         (0 nil)
         (1 (eq (agent-switch-section-type section) 'client))
         (_ t))))
   agent-switch--sections)
  (agent-switch-refresh t)
  ;; Expanding a previously collapsed Client reveals Profile sections that
  ;; were not present in the first section map.
  (when (= agent-switch--cycle-state 2)
    (maphash (lambda (id _section) (agent-switch--set-visible id t))
             agent-switch--sections)
    (agent-switch-refresh t)))

(defun agent-switch--visible-sections (&optional type parent)
  "Return visible sections, optionally restricted by TYPE and PARENT."
  (let (sections)
    (maphash (lambda (_id section)
               (when (and (or (null type)
                              (eq type (agent-switch-section-type section)))
                          (or (null parent)
                              (equal parent (agent-switch-section-parent section))))
                 (push section sections)))
             agent-switch--sections)
    (sort sections (lambda (left right)
                     (< (agent-switch-section-start left)
                        (agent-switch-section-start right))))))

(defun agent-switch--move-section (direction &optional sibling)
  "Move to another section in DIRECTION.
When SIBLING is non-nil, restrict movement to the same parent."
  (let* ((current (agent-switch--section-at-point t))
         (sections (agent-switch--visible-sections
                    nil (and sibling current
                             (agent-switch-section-parent current))))
         (position (and current (cl-position current sections :test #'eq)))
         (next (and position (+ position direction))))
    (unless (and next (>= next 0) (< next (length sections)))
      (user-error "No %s section" (if (> direction 0) "next" "previous")))
    (goto-char (agent-switch-section-start (nth next sections)))))

(defun agent-switch-next-section ()
  "Move to the next visible section."
  (interactive)
  (agent-switch--move-section 1))

(defun agent-switch-previous-section ()
  "Move to the previous visible section."
  (interactive)
  (agent-switch--move-section -1))

(defun agent-switch-next-sibling-section ()
  "Move to the next visible sibling section."
  (interactive)
  (agent-switch--move-section 1 t))

(defun agent-switch-previous-sibling-section ()
  "Move to the previous visible sibling section."
  (interactive)
  (agent-switch--move-section -1 t))

(defun agent-switch--profile-at-point (&optional noerror)
  "Return Profile section value at point, optionally NOERROR."
  (let ((section (agent-switch--section-at-point t)))
    (if (and section (eq (agent-switch-section-type section) 'profile))
        (agent-switch-section-value section)
      (unless noerror (user-error "No Profile at point")))))

(defun agent-switch--client-at-point (&optional noerror)
  "Return Client associated with point, optionally NOERROR."
  (let ((section (agent-switch--section-at-point t)))
    (cond
     ((and section (eq (agent-switch-section-type section) 'client))
      (agent-switch-section-value section))
     ((and section (eq (agent-switch-section-type section) 'profile))
      (agent-switch-get-client
       (agent-switch-profile-client-id (agent-switch-section-value section))))
     ((not noerror) (user-error "No Client at point")))))

(defun agent-switch-profile-details ()
  "Show secret-safe Profile details in a separate buffer."
  (interactive)
  (let* ((profile (agent-switch--profile-at-point))
         (client (agent-switch-get-client
                  (agent-switch-profile-client-id profile))))
    (with-current-buffer (get-buffer-create "*agent-switch profile*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize (agent-switch-profile-name profile)
                            'face 'agent-switch-title)
                "\n\n")
        (agent-switch--insert-profile-details client profile)
        (special-mode))
      (display-buffer (current-buffer)))))

(defun agent-switch-return ()
  "Open Profile details, or toggle a non-Profile section."
  (interactive)
  (if (agent-switch--profile-at-point t)
      (agent-switch-profile-details)
    (agent-switch-toggle-section)))

(defun agent-switch--job-running-p (client-id)
  "Return non-nil when CLIENT-ID has a running activation."
  (gethash client-id agent-switch--running-jobs))

(defun agent-switch--ensure-client-idle (client)
  "Signal when CLIENT already has a mutating operation in progress."
  (when (agent-switch--job-running-p (agent-switch-client-id client))
    (user-error "%s already has an operation in progress"
                (agent-switch-client-name client))))

(defun agent-switch--run-client-operation (client description result)
  "Track CLIENT operation DESCRIPTION when RESULT is a Job.
Refresh immediately for direct values."
  (if (not (agent-switch-job-p result))
      (progn (agent-switch-refresh-dashboards) result)
    (let ((client-id (agent-switch-client-id client))
          (buffer (current-buffer)))
      (puthash client-id result agent-switch--running-jobs)
      (agent-switch-refresh t)
      (agent-switch-job-start
       result
       (lambda (value)
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (remhash client-id agent-switch--running-jobs)))
         (agent-switch-refresh-dashboards)
         (message "%s complete" description)
         value)
       (lambda (error-value)
         (let ((message-text (agent-switch--safe-error-message error-value)))
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (remhash client-id agent-switch--running-jobs)
               (agent-switch-refresh t)))
           (message "agent-switch: %s" message-text)))))))

(defun agent-switch--activate-profile (client profile)
  "Activate CLIENT PROFILE transactionally from the dashboard."
  (let* ((client-id (agent-switch-client-id client))
         (adapter (agent-switch-get-adapter
                   (agent-switch-client-adapter-id client))))
    (unless (agent-switch-profile-valid-p profile)
      (user-error "%s" (agent-switch-profile-error profile)))
    (agent-switch--ensure-client-idle client)
    (unless (or (and (agent-switch-adapter-capability-p adapter :snapshot)
                     (agent-switch-adapter-capability-p adapter :rollback))
                (agent-switch-state-unprotected-confirmed-p
                 (agent-switch-adapter-id adapter))
                (and (yes-or-no-p
                      (format "%s has no automatic recovery; continue? "
                              (agent-switch-adapter-name adapter)))
                     (progn
                       (agent-switch-state-confirm-unprotected
                        (agent-switch-adapter-id adapter))
                       t)))
      (user-error "Cancelled"))
    (let ((job (agent-switch-activation-job client profile t))
          (buffer (current-buffer)))
      (puthash client-id job agent-switch--running-jobs)
      (agent-switch-refresh t)
      (agent-switch-job-start
       job
       (lambda (_value)
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (remhash client-id agent-switch--running-jobs)))
         (agent-switch-refresh-dashboards)
         (message "Activated %s for %s"
                  (agent-switch-profile-name profile)
                  (agent-switch-client-name client)))
       (lambda (error-value)
         (let ((message-text (agent-switch--safe-error-message error-value)))
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (remhash client-id agent-switch--running-jobs)
               (agent-switch-refresh t)))
           (message "agent-switch: %s" message-text)))))))

(defun agent-switch-activate-at-point ()
  "Activate the Profile at point transactionally."
  (interactive)
  (let* ((profile (agent-switch--profile-at-point))
         (client (agent-switch-get-client
                  (agent-switch-profile-client-id profile))))
    (agent-switch--activate-profile client profile)))

(defun agent-switch-reapply-last-selected ()
  "Reapply the last selected Profile for the Client at point."
  (interactive)
  (let* ((client (agent-switch--client-at-point))
         (client-id (agent-switch-client-id client))
         (profile-id (agent-switch-state-last-selected client-id)))
    (unless profile-id
      (user-error "%s has no last selected Profile"
                  (agent-switch-client-name client)))
    (agent-switch--activate-profile
     client (agent-switch-find-profile client-id profile-id))))

(defun agent-switch--random-profile-id (client-id)
  "Return an unused short random Profile ID for CLIENT-ID."
  (let (candidate)
    (while
        (progn
          (setq candidate
                (concat
                 "p-"
                 (substring
                  (secure-hash
                   'sha256
                   (format "%s:%s:%s:%s"
                           client-id (float-time) (random) (emacs-pid)))
                  0 8)))
          (file-exists-p (agent-switch-profile-path client-id candidate))))
    candidate))

(defun agent-switch--read-profile-name (&optional default)
  "Read a non-empty Profile name, using DEFAULT when supplied."
  (let ((name (string-trim
               (read-string "Profile name: " nil nil default))))
    (when (string-empty-p name)
      (user-error "Profile name is required"))
    name))

(defun agent-switch--new-profile-payload (client)
  "Return a fresh payload template for CLIENT."
  (let* ((adapter (agent-switch-get-adapter
                   (agent-switch-client-adapter-id client)))
         (template (agent-switch-adapter-callback adapter :profile-template))
         (payload (if template
                      (funcall template client nil)
                    (make-hash-table :test #'equal))))
    (unless (hash-table-p payload)
      (signal 'agent-switch-validation-error
              '("profile-template must return a JSON object")))
    (agent-switch-json-copy payload)))

(defun agent-switch--save-new-profile (client name payload)
  "Create, save, and visit a managed Profile for CLIENT."
  (let* ((client-id (agent-switch-client-id client))
         (id (agent-switch--random-profile-id client-id))
         (profile (agent-switch--make-profile
                   :id id :client-id client-id :name name
                   :description nil :payload payload
                   :ownership 'managed :valid-p t)))
    (agent-switch-save-profile profile)
    (agent-switch-refresh-dashboards)
    (find-file (agent-switch-profile-source profile))
    profile))

(defun agent-switch-profile-new ()
  "Create and visit a new managed Profile for the Client at point."
  (interactive)
  (let ((client (or (agent-switch--client-at-point t)
                    (agent-switch-get-client
                     (completing-read
                      "Client: "
                      (mapcar #'agent-switch-client-id
                              (agent-switch-clients))
                      nil t)))))
    (agent-switch--ensure-client-idle client)
    (agent-switch--save-new-profile
     client
     (agent-switch--read-profile-name
      (format "%s Profile" (agent-switch-client-name client)))
     (agent-switch--new-profile-payload client))))

(defun agent-switch--profile-at-point-noerror ()
  "Return the Profile at point, or nil when point is not in one."
  (condition-case nil
      (agent-switch--profile-at-point)
    (error nil)))

(defun agent-switch--effective-profile-for-client (client)
  "Return the managed Profile that Edit should open for CLIENT."
  (let* ((client-id (agent-switch-client-id client))
         (current-result (agent-switch--client-current client))
         (current (nth 0 current-result))
         (error-text (nth 1 current-result))
         (loading-p (nth 2 current-result))
         (profiles (agent-switch-profiles client-id))
         (matching
          (and current
               (cl-find-if
                (lambda (profile)
                  (and (agent-switch-profile-valid-p profile)
                       (condition-case nil
                           (agent-switch-profile-current-p
                            client profile current nil)
                         (error nil))))
                profiles)))
         (last-id (agent-switch-state-last-selected client-id)))
    (when loading-p
      (user-error "Current state is still loading"))
    (when error-text
      (user-error "%s" error-text))
    (or matching
        (and last-id (agent-switch-find-profile client-id last-id t))
        (user-error "No managed Profile matches current state; use Import Current"))))

(defun agent-switch-profile-edit ()
  "Visit the selected or effective managed Profile JSON."
  (interactive)
  (let* ((profile (or (agent-switch--profile-at-point-noerror)
                      (agent-switch--effective-profile-for-client
                       (agent-switch--client-at-point))))
         (source (agent-switch-profile-source profile)))
    (unless (eq (agent-switch-profile-ownership profile) 'managed)
      (user-error "Read-only Profile; use Copy first"))
    (unless (stringp source)
      (user-error "Profile has no managed source file"))
    (find-file source)))

(defun agent-switch--open-imported-profile (client payload)
  "Save captured PAYLOAD as a new managed Profile for CLIENT."
  (unless (hash-table-p payload)
    (signal 'agent-switch-validation-error
            '("capture-current must return a Profile payload object")))
  (agent-switch--save-new-profile
   client
   (agent-switch--read-profile-name
    (format "Imported %s" (agent-switch-client-name client)))
   payload))

(defun agent-switch-import-current ()
  "Import current Client state as a new managed Profile."
  (interactive)
  (let* ((client (agent-switch--client-at-point))
         (adapter (agent-switch-get-adapter
                   (agent-switch-client-adapter-id client)))
         (capture (agent-switch-adapter-callback adapter :capture-current))
         (current-result (agent-switch--client-current client))
         (current (nth 0 current-result))
         (error-text (nth 1 current-result))
         (loading-p (nth 2 current-result)))
    (agent-switch--ensure-client-idle client)
    (unless capture
      (user-error "%s cannot import current state"
                  (agent-switch-adapter-name adapter)))
    (when loading-p
      (user-error "Current state is still loading"))
    (when error-text
      (user-error "%s" error-text))
    (let ((result (funcall capture client current nil))
          (buffer (current-buffer)))
      (if (agent-switch-job-p result)
          (agent-switch-job-start
           result
           (lambda (payload)
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (agent-switch--open-imported-profile client payload))))
           (lambda (error-value)
             (message "agent-switch: %s"
                      (agent-switch--safe-error-message error-value))))
        (agent-switch--open-imported-profile client result)))))

(defun agent-switch-profile-copy ()
  "Copy the Profile at point to a new managed Profile and visit it."
  (interactive)
  (let* ((profile (agent-switch--profile-at-point))
         (client (agent-switch-get-client
                  (agent-switch-profile-client-id profile))))
    (agent-switch--ensure-client-idle client)
    (unless (agent-switch-profile-valid-p profile)
      (user-error "Invalid Profile cannot be copied safely"))
    (agent-switch--save-new-profile
     client
     (concat (agent-switch-profile-name profile) " Copy")
     (agent-switch-json-copy (agent-switch-profile-payload profile)))))

(defun agent-switch-profile-delete ()
  "Delete the inactive managed Profile at point."
  (interactive)
  (let* ((profile (agent-switch--profile-at-point))
         (client-id (agent-switch-profile-client-id profile))
         (client (agent-switch-get-client client-id))
         (current-result (agent-switch--client-current client))
         (current (nth 0 current-result))
         (current-p
          (and current
               (condition-case nil
                   (agent-switch-profile-current-p client profile current nil)
                 (error nil))))
         (last-selected (agent-switch-state-last-selected client-id))
         (source (agent-switch-profile-source profile))
         (visiting (and (stringp source) (find-buffer-visiting source))))
    (agent-switch--ensure-client-idle client)
    (unless (eq (agent-switch-profile-ownership profile) 'managed)
      (user-error "Read-only Profiles cannot be deleted"))
    (when (or current-p
              (equal (agent-switch-profile-id profile) last-selected))
      (user-error "Current Profile cannot be deleted; Apply another Profile first"))
    (when (and visiting (buffer-modified-p visiting))
      (user-error "Profile has unsaved changes in %s" (buffer-name visiting)))
    (unless (yes-or-no-p
             (format "Delete managed Profile %s? "
                     (agent-switch-profile-name profile)))
      (user-error "Cancelled"))
    (agent-switch-delete-profile profile)
    (agent-switch-refresh-dashboards)))

(defun agent-switch--move-profile (direction)
  "Move Profile at point by DIRECTION in state order."
  (let* ((profile (agent-switch--profile-at-point))
         (client-id (agent-switch-profile-client-id profile))
         (profiles (agent-switch-profiles client-id))
         (ids (mapcar #'agent-switch-profile-id profiles))
         (position (cl-position (agent-switch-profile-id profile) ids
                                :test #'equal))
         (target (+ position direction)))
    (agent-switch--ensure-client-idle (agent-switch-get-client client-id))
    (unless (and (>= target 0) (< target (length ids)))
      (user-error "Profile is already at that edge"))
    (cl-rotatef (nth position ids) (nth target ids))
    (agent-switch-state-set-profile-order client-id ids)
    (agent-switch-refresh-dashboards)))

(defun agent-switch-profile-move-up ()
  "Move Profile at point up."
  (interactive)
  (agent-switch--move-profile -1))

(defun agent-switch-profile-move-down ()
  "Move Profile at point down."
  (interactive)
  (agent-switch--move-profile 1))

(defun agent-switch-open-profile-file ()
  "Open managed Profile JSON at point."
  (interactive)
  (let ((profile (agent-switch--profile-at-point)))
    (unless (stringp (agent-switch-profile-source profile))
      (user-error "Profile has no managed source file"))
    (find-file (agent-switch-profile-source profile))))

(defun agent-switch-diagnose ()
  "Display sanitized agent-switch diagnostics."
  (interactive)
  (let ((lines
         (list
          (format "Data directory: %s" (agent-switch--directory))
          (format "State file: %s" (agent-switch-state-path))
          (format "State status: %s"
                  (or (agent-switch-state-record-error (agent-switch-read-state))
                      "ok"))
          (format "Registered clients: %s"
                  (string-join (mapcar #'agent-switch-client-id
                                       (agent-switch-clients)) ", "))
          (format "toml available: %s" (if (locate-library "toml") "yes" "no"))
          (format "tomelr available: %s" (if (locate-library "tomelr") "yes" "no"))
          (format "gptel available: %s" (if (locate-library "gptel") "yes" "no")))))
    (with-current-buffer (get-buffer-create "*agent-switch diagnose*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (string-join lines "\n") "\n")
        (special-mode))
      (display-buffer (current-buffer)))))

(transient-define-prefix agent-switch-menu ()
  "Open the agent-switch action menu."
  [["Profile"
    ("a" "Apply" agent-switch-activate-at-point)
    ("n" "New" agent-switch-profile-new)
    ("c" "Copy" agent-switch-profile-copy)
    ("e" "Edit" agent-switch-profile-edit)
    ("D" "Delete" agent-switch-profile-delete)]
   ["View"
    ("g" "Refresh" agent-switch-refresh)
    ("RET" "Details" agent-switch-profile-details)
    ("d" "Diagnose" agent-switch-diagnose)
    ("r" "Reset damaged state" agent-switch-reset-state)]
   ["Order"
    ("<up>" "Move up" agent-switch-profile-move-up :transient t)
    ("<down>" "Move down" agent-switch-profile-move-down :transient t)]])

(defvar agent-switch-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "TAB") #'agent-switch-toggle-section)
    (define-key map (kbd "<tab>") #'agent-switch-toggle-section)
    (define-key map (kbd "<backtab>") #'agent-switch-cycle-sections)
    (define-key map (kbd "<S-tab>") #'agent-switch-cycle-sections)
    (define-key map (kbd "RET") #'agent-switch-return)
    (define-key map (kbd "?") #'agent-switch-menu)
    (define-key map (kbd "q") #'quit-window)
    ;; Evil's state maps intentionally retain these keys in normal state.
    (define-key map (kbd "g") #'agent-switch-refresh)
    (define-key map (kbd "n") #'agent-switch-next-section)
    (define-key map (kbd "p") #'agent-switch-previous-section)
    (define-key map (kbd "M-n") #'agent-switch-next-sibling-section)
    (define-key map (kbd "M-p") #'agent-switch-previous-sibling-section)
    map)
  "Keymap for `agent-switch-mode'.")

(defun agent-switch--schedule-watch-refresh (&rest _ignore)
  "Debounce a dashboard refresh after an external change."
  (when (timerp agent-switch--watch-timer)
    (cancel-timer agent-switch--watch-timer))
  (let ((buffer (current-buffer)))
    (setq agent-switch--watch-timer
          (run-at-time
           agent-switch-watch-debounce nil
           (lambda ()
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (setq agent-switch--watch-timer nil)
                 (agent-switch-refresh))))))))

(defun agent-switch--watch-callback (buffer _event)
  "Handle a file notification event for dashboard BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (agent-switch--schedule-watch-refresh))))

(defun agent-switch--watchable-path (path)
  "Return existing PATH or its nearest existing parent directory."
  (let ((candidate (expand-file-name path)))
    (while (and candidate (not (file-exists-p candidate)))
      (let ((parent (file-name-directory (directory-file-name candidate))))
        (setq candidate (unless (or (null parent) (equal parent candidate))
                          parent))))
    candidate))

(defun agent-switch--install-watchers ()
  "Install file and runtime watchers for the current dashboard."
  (let ((buffer (current-buffer)))
    (when-let* ((watch-path
                 (agent-switch--watchable-path
                  (agent-switch-profiles-directory))))
      (push (file-notify-add-watch
             watch-path '(change attribute-change)
             (lambda (event) (agent-switch--watch-callback buffer event)))
            agent-switch--watch-descriptors))
    (dolist (client (agent-switch-clients))
      (let* ((adapter (agent-switch-get-adapter
                       (agent-switch-client-adapter-id client)))
             (paths-fn (agent-switch-adapter-callback adapter :watch-paths))
             (setup-fn (agent-switch-adapter-callback adapter :watch-setup)))
        (when paths-fn
          (condition-case nil
              (let ((paths (funcall paths-fn client nil)))
                (unless (agent-switch-job-p paths)
                  (dolist (path paths)
                    (when-let* ((watch-path (agent-switch--watchable-path path)))
                      (push (file-notify-add-watch
                             watch-path '(change attribute-change)
                             (lambda (event)
                               (agent-switch--watch-callback buffer event)))
                            agent-switch--watch-descriptors)))))
            (error nil)))
        (when setup-fn
          (condition-case nil
              (let ((cleanup (funcall setup-fn
                                      client
                                      (lambda (&rest _ignore)
                                        (when (buffer-live-p buffer)
                                          (with-current-buffer buffer
                                            (agent-switch--schedule-watch-refresh)))))))
                (when (functionp cleanup)
                  (push cleanup agent-switch--watch-cleanups)))
            (error nil)))))))

(defun agent-switch--cleanup ()
  "Remove watchers, timers, and running jobs owned by this dashboard."
  (agent-switch--clear-section-highlight)
  (dolist (descriptor agent-switch--watch-descriptors)
    (ignore-errors (file-notify-rm-watch descriptor)))
  (dolist (cleanup agent-switch--watch-cleanups)
    (ignore-errors (funcall cleanup)))
  (when (timerp agent-switch--watch-timer)
    (cancel-timer agent-switch--watch-timer))
  (maphash (lambda (_client-id job)
             (agent-switch-job-cancel job))
           agent-switch--running-jobs)
  (maphash (lambda (_client-id entry)
             (when-let* ((job (and (eq (plist-get entry :status) 'pending)
                                    (plist-get entry :job))))
               (agent-switch-job-cancel job)))
           agent-switch--current-cache)
  (setq agent-switch--watch-descriptors nil
        agent-switch--watch-cleanups nil
        agent-switch--watch-timer nil))

(define-derived-mode agent-switch-mode special-mode "Agent-Switch"
  "Major mode for the agent-switch section dashboard."
  (setq-local truncate-lines t)
  (setq-local agent-switch--sections (make-hash-table :test #'equal))
  (setq-local agent-switch--visibility (make-hash-table :test #'equal))
  (setq-local agent-switch--current-cache (make-hash-table :test #'equal))
  (kill-local-variable 'agent-switch--running-jobs)
  (unless (hash-table-p agent-switch--running-jobs)
    (setq-default agent-switch--running-jobs
                  (make-hash-table :test #'equal)))
  (setq-local revert-buffer-function
              (lambda (&rest _ignore) (agent-switch-refresh)))
  (add-hook 'post-command-hook #'agent-switch--update-section-highlight nil t)
  (add-hook 'kill-buffer-hook #'agent-switch--cleanup nil t)
  (agent-switch--install-watchers))

(with-eval-after-load 'evil
  (evil-set-initial-state 'agent-switch-mode 'normal)
  ;; Only shared structural/action keys are installed in Evil normal state.
  ;; g, n/p, M-n/M-p, and mutation keys remain native or transient-only.
  (dolist (binding '(("TAB" . agent-switch-toggle-section)
                     ("<tab>" . agent-switch-toggle-section)
                     ("<backtab>" . agent-switch-cycle-sections)
                     ("<S-tab>" . agent-switch-cycle-sections)
                     ("RET" . agent-switch-return)
                     ("?" . agent-switch-menu)
                     ("q" . quit-window)))
    (evil-define-key* 'normal agent-switch-mode-map
      (kbd (car binding)) (cdr binding))))

;;;###autoload
(defun agent-switch-dashboard ()
  "Create or show the agent-switch dashboard."
  (interactive)
  (let ((buffer (get-buffer-create agent-switch-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'agent-switch-mode)
        (agent-switch-mode))
      (agent-switch-refresh))
    (pop-to-buffer buffer)))
(provide 'agent-switch-ui)

;;; agent-switch-ui.el ends here
