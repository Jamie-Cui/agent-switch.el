;;; agent-switch-ui.el --- Section dashboard and profile editor -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Section dashboard, optional Evil integration, transient actions, file
;; watchers, semantic faces, and managed Profile file editing.

;;; Code:

(require 'cl-lib)
(require 'filenotify)
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

(defface agent-switch-section-heading
  '((t :inherit bold))
  "Section heading face."
  :group 'agent-switch)

(defface agent-switch-profile-row
  '((t :inherit fixed-pitch))
  "Provider Profile row face."
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

(cl-defstruct (agent-switch-section
               (:constructor agent-switch--make-section))
  id type start end value expanded-p)

(cl-defstruct (agent-switch-client-view
               (:constructor agent-switch--make-client-view))
  client profiles current current-profile last-selected error loading-p)

(defvar-local agent-switch--sections nil)
(defvar-local agent-switch--visibility nil)
(defvar-local agent-switch--generation 0)
(defvar-local agent-switch--current-cache nil)
(defvar agent-switch--running-jobs (make-hash-table :test #'equal)
  "Mutating Client Jobs shared by all agent-switch UI buffers.")

(defvar-local agent-switch--watch-descriptors nil)
(defvar-local agent-switch--watch-cleanups nil)
(defvar-local agent-switch--watch-timer nil)

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
    (id type label &optional value face)
  "Insert a section heading and register it.
ID and TYPE describe the section.  LABEL is displayed, VALUE is associated
context, and FACE overrides the standard heading face."
  (let* ((expanded (agent-switch--visibility-value id type))
         (start (point))
         (section (agent-switch--make-section
                   :id id :type type :start start
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
           'help-echo (if (eq type 'client)
                          "TAB toggles this Client"
                        "RET edits this Profile")))
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

(defun agent-switch--client-view (client)
  "Build an isolated dashboard view model for CLIENT."
  (let* ((client-id (agent-switch-client-id client))
         (last-selected (condition-case nil
                            (agent-switch-state-last-selected client-id)
                          (error nil)))
         (profiles nil)
         (profile-error nil)
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
    (agent-switch--make-client-view
     :client client
     :profiles profiles
     :current current
     :current-profile (agent-switch--matching-profile
                       client profiles current last-selected)
     :last-selected last-selected
     :error (or current-error profile-error)
     :loading-p loading-p)))

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
         (applied-payload
          (and (hash-table-p applied) (gethash "payload" applied)))
         (profile-changed
          (and last-profile (hash-table-p applied-payload)
               (not (agent-switch--json-value-equal-p
                     (agent-switch-profile-payload last-profile)
                     applied-payload))))
         (live-matches-applied
          (and current last-profile (hash-table-p applied-payload)
               (let ((snapshot (copy-agent-switch-profile last-profile)))
                 (setf (agent-switch-profile-payload snapshot) applied-payload)
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

(defun agent-switch--insert-profile-section (view profile)
  "Insert PROFILE section for client VIEW."
  (let* ((client-id (agent-switch-client-id
                     (agent-switch-client-view-client view)))
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
         (section
          (agent-switch--insert-section-heading
           id 'profile label profile
           (if current-p 'agent-switch-current
             'agent-switch-profile-row))))
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
                   id 'client label client 'default)))
    (when (agent-switch-section-expanded-p section)
      (when-let* ((error-text (agent-switch-client-view-error view)))
        (agent-switch--insert-detail-line
         "Error" error-text 'agent-switch-status-error))
      (if (agent-switch-client-view-profiles view)
          (dolist (profile (agent-switch-client-view-profiles view))
            (agent-switch--insert-profile-section view profile))
        (insert (propertize "    No profiles\n" 'face 'shadow))))
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
    (agent-switch--restore-position section-id line window-start)))

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
  "Toggle the Client section at point."
  (interactive)
  (let ((section (agent-switch--section-at-point)))
    (unless (eq (agent-switch-section-type section) 'client)
      (user-error "Profile rows are not collapsible"))
    (agent-switch--set-visible
     (agent-switch-section-id section)
     (not (agent-switch-section-expanded-p section)))
    (agent-switch-refresh t)))

(defun agent-switch--visible-sections ()
  "Return visible sections in display order."
  (let (sections)
    (maphash (lambda (_id section)
               (push section sections))
             agent-switch--sections)
    (sort sections (lambda (left right)
                     (< (agent-switch-section-start left)
                        (agent-switch-section-start right))))))

(defun agent-switch--move-section (direction)
  "Move to another section in DIRECTION."
  (let* ((current (agent-switch--section-at-point t))
         (sections (agent-switch--visible-sections))
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

(defun agent-switch-return ()
  "Edit the Profile at point, or toggle a non-Profile section."
  (interactive)
  (if (agent-switch--profile-at-point t)
      (agent-switch-profile-edit)
    (agent-switch-toggle-section)))

(defun agent-switch--job-running-p (client-id)
  "Return non-nil when CLIENT-ID has a running activation."
  (gethash client-id agent-switch--running-jobs))

(defun agent-switch--ensure-client-idle (client)
  "Signal when CLIENT already has a mutating operation in progress."
  (when (agent-switch--job-running-p (agent-switch-client-id client))
    (user-error "%s already has an operation in progress"
                (agent-switch-client-name client))))

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
                   :payload payload
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

(defun agent-switch-profile-edit ()
  "Visit the selected managed Profile JSON."
  (interactive)
  (let* ((profile (agent-switch--profile-at-point))
         (source (agent-switch-profile-source profile)))
    (unless (eq (agent-switch-profile-ownership profile) 'managed)
      (user-error "Read-only Profile; use Copy first"))
    (unless (stringp source)
      (user-error "Profile has no managed source file"))
    (find-file source)))

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
    ("d" "Diagnose" agent-switch-diagnose)
    ("r" "Reset damaged state" agent-switch-reset-state)]])

(defvar agent-switch-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "TAB") #'agent-switch-toggle-section)
    (define-key map (kbd "<tab>") #'agent-switch-toggle-section)
    (define-key map (kbd "RET") #'agent-switch-return)
    (define-key map (kbd "?") #'agent-switch-menu)
    (define-key map (kbd "q") #'quit-window)
    ;; Evil's state maps intentionally retain these keys in normal state.
    (define-key map (kbd "g") #'agent-switch-refresh)
    (define-key map (kbd "n") #'agent-switch-next-section)
    (define-key map (kbd "p") #'agent-switch-previous-section)
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
  (setq-local revert-buffer-function
              (lambda (&rest _ignore) (agent-switch-refresh)))
  (add-hook 'kill-buffer-hook #'agent-switch--cleanup nil t)
  (hl-line-mode 1)
  (agent-switch--install-watchers))

(with-eval-after-load 'evil
  (evil-set-initial-state 'agent-switch-mode 'normal)
  ;; Only shared structural/action keys are installed in Evil normal state.
  ;; g, n/p, and mutation keys remain native or transient-only.
  (dolist (binding '(("TAB" . agent-switch-toggle-section)
                     ("<tab>" . agent-switch-toggle-section)
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
