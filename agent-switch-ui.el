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
(require 'wid-edit)
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
  client profiles current current-profile last-selected error loading-p)

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
(defvar-local agent-switch--last-error nil)

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

(defun agent-switch--section-indicator (expanded)
  "Return a compact visibility indicator for EXPANDED."
  (if (and (char-displayable-p ?\u25b8)
           (char-displayable-p ?\u25be))
      (if expanded "▾" "▸")
    (if expanded "-" "+")))

(defun agent-switch--insert-section-heading
    (id type parent label &optional value face)
  "Insert a section heading and register it.
ID, TYPE, and PARENT describe the section.  LABEL is displayed, VALUE is
associated context, and FACE overrides the standard heading face."
  (let* ((expanded (agent-switch--visibility-value id type))
         (start (point))
         (indicator (agent-switch--section-indicator expanded))
         (section (agent-switch--make-section
                   :id id :type type :parent parent :start start
                   :value value :expanded-p expanded)))
    (insert (propertize indicator 'face 'shadow) " ")
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
  (let ((current-profile (agent-switch-client-view-current-profile view))
        (last-selected (agent-switch-client-view-last-selected view))
        (client-id (agent-switch-client-id
                    (agent-switch-client-view-client view))))
    (cond
     ((agent-switch--job-running-p client-id)
      (propertize "working" 'face 'agent-switch-status-warning))
     ((agent-switch-client-view-loading-p view)
      (propertize "loading" 'face 'shadow))
     ((agent-switch-client-view-error view)
      (propertize "error" 'face 'agent-switch-status-error))
     ((and current-profile
           (equal (agent-switch-profile-id current-profile) last-selected))
      (concat "current "
              (propertize (agent-switch-profile-name current-profile)
                          'face 'agent-switch-current)))
     (current-profile
      (concat (propertize "changed externally: "
                          'face 'agent-switch-status-warning)
              (agent-switch-profile-name current-profile)))
     ((agent-switch-client-view-current view)
      (propertize "changed externally" 'face 'agent-switch-status-warning))
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
     "Data" (abbreviate-file-name (agent-switch--directory))
     'agent-switch-secondary)
    (agent-switch--insert-status-line
     "Clients" (number-to-string (length (agent-switch-clients))))
    (agent-switch--insert-status-line
     "State" (if state-error "damaged; read-only until reset" "ok")
     (if state-error 'agent-switch-status-error
       'agent-switch-status-success))
    (when state-error
      (agent-switch--insert-status-line
       "Error" state-error 'agent-switch-status-error))
    (when agent-switch--last-error
      (agent-switch--insert-status-line
       "Last operation" agent-switch--last-error
       'agent-switch-status-error))))

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
         (marker (if current-p "*" " "))
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
         (name (propertize
                (agent-switch--display-width
                 (agent-switch-client-name client)
                 (max 8 agent-switch-client-name-width))
                'face 'agent-switch-section-heading))
         (label (concat name
                        "  " (agent-switch--client-status view)))
         (section (agent-switch--insert-section-heading
                   id 'client nil label client 'default)))
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
       (setq agent-switch--last-error
             (agent-switch--safe-error-message error-value))
       (insert (propertize agent-switch--last-error
                           'face 'agent-switch-status-error)
               "\n")))
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
               (setq agent-switch--last-error message-text)
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
             (remhash client-id agent-switch--running-jobs)
             (setq agent-switch--last-error nil)))
         (agent-switch-refresh-dashboards)
         (message "Activated %s for %s"
                  (agent-switch-profile-name profile)
                  (agent-switch-client-name client)))
       (lambda (error-value)
         (let ((message-text (agent-switch--safe-error-message error-value)))
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (remhash client-id agent-switch--running-jobs)
               (setq agent-switch--last-error message-text)
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

(defun agent-switch--open-captured-profile (client payload)
  "Open a managed Profile form for CLIENT using captured PAYLOAD."
  (unless (hash-table-p payload)
    (signal 'agent-switch-validation-error
            '("capture-current must return a Profile payload object")))
  (let ((profile (agent-switch--make-profile
                  :id "captured" :client-id (agent-switch-client-id client)
                  :name (format "Captured %s" (agent-switch-client-name client))
                  :payload payload :ownership 'managed :valid-p t)))
    (agent-switch--open-profile-form client profile t)))

(defun agent-switch-adopt-current ()
  "Capture current Client state into a new managed Profile form."
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
      (user-error "%s cannot capture current state"
                  (agent-switch-adapter-name adapter)))
    (when loading-p (user-error "Current state is still loading"))
    (when error-text (user-error "%s" error-text))
    (let ((result (funcall capture client current nil))
          (buffer (current-buffer)))
      (if (agent-switch-job-p result)
          (agent-switch-job-start
           result
           (lambda (payload)
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (agent-switch--open-captured-profile client payload))))
           (lambda (error-value)
             (message "agent-switch: %s"
                      (agent-switch--safe-error-message error-value))))
        (agent-switch--open-captured-profile client result)))))

(defun agent-switch--profile-field-value (profile field)
  "Return PROFILE value described by FIELD."
  (agent-switch-json-get-in (agent-switch-profile-payload profile)
                            (plist-get field :path)))

(defun agent-switch--secret-reference-display (value)
  "Return editable text for secret reference VALUE."
  (if (not (agent-switch-secret-reference-p value))
      ""
    (pcase (gethash "source" value)
      ("env" (concat "env:" (gethash "name" value)))
      ("auth-source"
       (string-join
        (delq nil (list "auth-source" (gethash "host" value)
                        (gethash "user" value))) ":"))
      (_ ""))))

(defun agent-switch--parse-secret-reference (text)
  "Parse secret reference TEXT into a JSON object."
  (cond
   ((string-prefix-p "env:" text)
    (let ((object (make-hash-table :test #'equal)))
      (puthash "source" "env" object)
      (puthash "name" (substring text 4) object)
      (unless (agent-switch-secret-reference-p object)
        (user-error "Expected env:VARIABLE"))
      object))
   ((string-prefix-p "auth-source:" text)
    (let* ((parts (split-string text ":"))
           (object (make-hash-table :test #'equal)))
      (puthash "source" "auth-source" object)
      (puthash "host" (nth 1 parts) object)
      (when-let* ((user (nth 2 parts))) (puthash "user" user object))
      (unless (agent-switch-secret-reference-p object)
        (user-error "Expected auth-source:HOST[:USER]"))
      object))
   (t (user-error "Use env:VARIABLE or auth-source:HOST[:USER]"))))

(defun agent-switch-profile-edit--mark-dirty (&rest _ignore)
  "Mark the current Profile form dirty."
  (setq agent-switch-profile-edit--dirty-p t))

(defun agent-switch--field-choices (field client profile)
  "Return FIELD choices for CLIENT and PROFILE."
  (let ((choices (plist-get field :choices)))
    (cond ((functionp choices) (funcall choices client profile))
          ((and (symbolp choices) (fboundp choices))
           (funcall choices client profile))
          (t choices))))

(defun agent-switch--insert-form-field (client profile field)
  "Insert a widget for CLIENT PROFILE FIELD and remember it."
  (let* ((type (plist-get field :type))
         (label (or (plist-get field :label) (plist-get field :key)))
         (value (agent-switch--profile-field-value profile field))
         (notify #'agent-switch-profile-edit--mark-dirty)
         widget)
    (widget-insert (format "%-28s " (concat label ":")))
    (setq widget
          (pcase type
            ('boolean
             (widget-create 'checkbox :value (eq value t) :notify notify))
            ('choice
             (let ((choices (agent-switch--field-choices field client profile)))
               (if choices
                   (apply #'widget-create
                          'menu-choice
                          :value value :notify notify
                          (mapcar (lambda (choice)
                                    `(item :tag ,(format "%s" choice)
                                           :value ,choice))
                                  choices))
                 (widget-create 'editable-field
                                :value (or value "") :notify notify))))
            ('integer
             (widget-create 'editable-field
                            :value (if (integerp value)
                                       (number-to-string value) "")
                            :notify notify))
            ('string-list
             (widget-create 'editable-field
                            :value (string-join (append value nil) ", ")
                            :notify notify))
            ('secret-reference
             (widget-create 'editable-field
                            :value (agent-switch--secret-reference-display value)
                            :notify notify))
            (_
             (widget-create 'editable-field
                            :value (if (stringp value) value "")
                            :notify notify))))
    (widget-insert "\n")
    (push (list :field field :widget widget) agent-switch-profile-edit--widgets)))

(defun agent-switch--make-edit-profile (client profile new-p id name description)
  "Return editable Profile for CLIENT based on PROFILE and form values.
NEW-P clears source identity; ID, NAME, and DESCRIPTION come from the form."
  (let ((result (copy-agent-switch-profile profile)))
    (setf (agent-switch-profile-id result) id
          (agent-switch-profile-client-id result) (agent-switch-client-id client)
          (agent-switch-profile-name result) name
          (agent-switch-profile-description result)
          (unless (string-empty-p description) description)
          (agent-switch-profile-ownership result) 'managed
          (agent-switch-profile-valid-p result) t)
    (when new-p
      (setf (agent-switch-profile-source result) nil
            (agent-switch-profile-source-hash result) nil))
    result))

(defun agent-switch-profile-edit--widget (key)
  "Return core form widget stored under KEY."
  (plist-get agent-switch-profile-edit--core-widgets key))

(defun agent-switch--field-widget-value (entry)
  "Convert form widget ENTRY to its JSON field value."
  (let* ((field (plist-get entry :field))
         (type (plist-get field :type))
         (raw (widget-value (plist-get entry :widget))))
    (pcase type
      ('integer
       (if (string-empty-p (string-trim raw)) nil
         (unless (string-match-p "\\`[-+]?[0-9]+\\'" (string-trim raw))
           (user-error "%s must be an integer" (plist-get field :label)))
         (string-to-number raw)))
      ('boolean (if raw t agent-switch-json-false))
      ('string-list
       (vconcat (split-string raw "[[:space:]]*,[[:space:]]*" t)))
      ('secret-reference
       (if (string-empty-p (string-trim raw)) nil
         (agent-switch--parse-secret-reference (string-trim raw))))
      (_ (if (and (stringp raw) (string-empty-p (string-trim raw)))
             nil raw)))))

(defun agent-switch-profile-edit--finish-save (buffer client profile)
  "Save CLIENT PROFILE when editor BUFFER is still live."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq agent-switch-profile-edit--saving-p t
            agent-switch-profile-edit--validation-job nil)
      (agent-switch-save-profile profile)
      (setq agent-switch-profile-edit--dirty-p nil)
      (agent-switch-refresh-dashboards)
      (kill-buffer buffer)
      (message "Saved Profile %s/%s"
               (agent-switch-client-id client)
               (agent-switch-profile-id profile)))))

(defun agent-switch-profile-edit-save ()
  "Validate and save the current Profile form."
  (interactive)
  (let* ((buffer (current-buffer))
         (client agent-switch-profile-edit--client)
         (id (string-trim
              (widget-value (agent-switch-profile-edit--widget :id))))
         (name (string-trim
                (widget-value (agent-switch-profile-edit--widget :name))))
         (description
          (string-trim
           (widget-value (agent-switch-profile-edit--widget :description))))
         (profile (agent-switch--make-edit-profile
                   client agent-switch-profile-edit--profile
                   agent-switch-profile-edit--new-p id name description))
         (payload (agent-switch-json-copy
                   (agent-switch-profile-payload profile))))
    (agent-switch--ensure-client-idle client)
    (unless (and (not (string-empty-p id)) (not (string-empty-p name)))
      (user-error "Profile ID and name are required"))
    (dolist (entry (cl-remove-if-not
                    (lambda (item) (plist-get item :field))
                    agent-switch-profile-edit--widgets))
      (let* ((field (plist-get entry :field))
             (path (plist-get field :path))
             (widget (plist-get entry :widget))
             value)
        (condition-case error-value
            (setq value (agent-switch--field-widget-value entry))
          (user-error
           (goto-char (widget-get widget :from))
           (signal (car error-value) (cdr error-value))))
        (when (and (plist-get field :required)
                   (or (null value)
                       (and (stringp value) (string-empty-p value))))
          (goto-char (widget-get widget :from))
          (user-error "%s is required" (plist-get field :label)))
        (if (null value)
            (agent-switch-json-remove-in payload path)
          (agent-switch-json-put-in payload path value))))
    (setf (agent-switch-profile-payload profile) payload)
    (let* ((adapter (agent-switch-get-adapter
                     (agent-switch-client-adapter-id client)))
           (validate (agent-switch-adapter-callback adapter :validate)))
      (let ((result (and validate (funcall validate client profile nil))))
        (if (agent-switch-job-p result)
            (progn
              (setq agent-switch-profile-edit--saving-p t
                    agent-switch-profile-edit--validation-job result)
              (message "Validating Profile %s..." id)
              (agent-switch-job-start
               result
               (lambda (_value)
                 (agent-switch-profile-edit--finish-save
                  buffer client profile))
               (lambda (error-value)
                 (when (buffer-live-p buffer)
                   (with-current-buffer buffer
                     (setq agent-switch-profile-edit--saving-p nil
                           agent-switch-profile-edit--validation-job nil))
                   (message "agent-switch validation: %s"
                            (agent-switch--safe-error-message error-value))))))
          (agent-switch-profile-edit--finish-save buffer client profile))))))

(defun agent-switch-profile-edit-cancel ()
  "Cancel the current Profile form."
  (interactive)
  (when (and agent-switch-profile-edit--dirty-p
             (not (yes-or-no-p "Discard unsaved Profile changes? ")))
    (user-error "Cancelled"))
  (when (agent-switch-job-p agent-switch-profile-edit--validation-job)
    (agent-switch-job-cancel agent-switch-profile-edit--validation-job))
  (setq agent-switch-profile-edit--dirty-p nil
        agent-switch-profile-edit--validation-job nil)
  (kill-buffer (current-buffer)))

(defun agent-switch-profile-edit--kill-query ()
  "Confirm killing a dirty Profile editor."
  (or agent-switch-profile-edit--saving-p
      (not agent-switch-profile-edit--dirty-p)
      (yes-or-no-p "Discard unsaved Profile changes? ")))

(defun agent-switch-profile-edit--cleanup ()
  "Cancel pending validation owned by the current Profile editor."
  (when (agent-switch-job-p agent-switch-profile-edit--validation-job)
    (agent-switch-job-cancel agent-switch-profile-edit--validation-job)))

(defun agent-switch-profile-edit-next-field ()
  "Move to the next form field and enter Evil insert state when available."
  (interactive)
  (widget-forward 1)
  (when (fboundp 'evil-insert-state) (evil-insert-state)))

(defun agent-switch-profile-edit-previous-field ()
  "Move to the previous form field and enter Evil insert state when available."
  (interactive)
  (widget-backward 1)
  (when (fboundp 'evil-insert-state) (evil-insert-state)))

(defvar agent-switch-profile-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "C-c C-c") #'agent-switch-profile-edit-save)
    (define-key map (kbd "C-c C-k") #'agent-switch-profile-edit-cancel)
    (define-key map (kbd "TAB") #'agent-switch-profile-edit-next-field)
    (define-key map (kbd "<tab>") #'agent-switch-profile-edit-next-field)
    (define-key map (kbd "<backtab>") #'agent-switch-profile-edit-previous-field)
    (define-key map (kbd "<S-tab>") #'agent-switch-profile-edit-previous-field)
    map)
  "Keymap for `agent-switch-profile-edit-mode'.")

(define-derived-mode agent-switch-profile-edit-mode special-mode
  "Agent-Profile-Edit"
  "Major mode for editing a managed agent-switch Profile."
  (setq-local truncate-lines nil)
  (setq-local buffer-read-only nil)
  (add-hook 'kill-buffer-query-functions
            #'agent-switch-profile-edit--kill-query nil t)
  (add-hook 'kill-buffer-hook #'agent-switch-profile-edit--cleanup nil t))

(defun agent-switch--open-profile-form (client profile new-p)
  "Open widget form for CLIENT PROFILE, marking it NEW-P."
  (let* ((adapter (agent-switch-get-adapter
                   (agent-switch-client-adapter-id client)))
         (editor (agent-switch-adapter-editor adapter))
         (fields (agent-switch-adapter-profile-fields adapter)))
    (cond
     (editor (funcall editor client profile new-p))
     ((not fields)
      (user-error "%s does not provide a Profile editor"
                  (agent-switch-adapter-name adapter)))
     (t
      (let ((buffer (get-buffer-create
                     (format "*agent-switch edit %s*"
                             (agent-switch-profile-id profile)))))
        (with-current-buffer buffer
          (agent-switch-profile-edit-mode)
          (let ((inhibit-read-only t))
            (erase-buffer)
            (remove-overlays)
            (setq agent-switch-profile-edit--client client
                  agent-switch-profile-edit--profile profile
                  agent-switch-profile-edit--new-p new-p
                  agent-switch-profile-edit--core-widgets nil
                  agent-switch-profile-edit--widgets nil
                  agent-switch-profile-edit--dirty-p nil
                  agent-switch-profile-edit--saving-p nil
                  agent-switch-profile-edit--validation-job nil)
            (widget-insert (propertize
                            (if new-p "Create Profile" "Edit Profile")
                            'face 'agent-switch-title)
                           "\n\n")
            (widget-insert (format "%-28s " "Profile ID:"))
            (let ((widget (if new-p
                              (widget-create 'editable-field
                                             :value (agent-switch-profile-id profile)
                                             :notify #'agent-switch-profile-edit--mark-dirty)
                            (widget-create 'item
                                           :value (agent-switch-profile-id profile)))))
              (setq agent-switch-profile-edit--core-widgets
                    (plist-put agent-switch-profile-edit--core-widgets
                               :id widget)))
            (widget-insert "\n")
            (widget-insert (format "%-28s " "Name:"))
            (setq agent-switch-profile-edit--core-widgets
                  (plist-put
                   agent-switch-profile-edit--core-widgets :name
                   (widget-create 'editable-field
                                  :value (agent-switch-profile-name profile)
                                  :notify #'agent-switch-profile-edit--mark-dirty)))
            (widget-insert "\n")
            (widget-insert (format "%-28s " "Description:"))
            (setq agent-switch-profile-edit--core-widgets
                  (plist-put
                   agent-switch-profile-edit--core-widgets :description
                   (widget-create 'editable-field
                                  :value (or (agent-switch-profile-description profile) "")
                                  :notify #'agent-switch-profile-edit--mark-dirty)))
            (widget-insert "\n\n")
            (dolist (field fields)
              (agent-switch--insert-form-field client profile field))
            (setq agent-switch-profile-edit--widgets
                  (nreverse agent-switch-profile-edit--widgets))
            (widget-insert "\n")
            (widget-create 'push-button
                           :notify (lambda (&rest _ignore)
                                     (agent-switch-profile-edit-save))
                           "Save")
            (widget-insert "  ")
            (widget-create 'push-button
                           :notify (lambda (&rest _ignore)
                                     (agent-switch-profile-edit-cancel))
                           "Cancel")
            (widget-setup)
            (goto-char (point-min))))
        (pop-to-buffer buffer))))))

(defun agent-switch-profile-create ()
  "Create a managed Profile for the Client at point."
  (interactive)
  (let* ((client (or (agent-switch--client-at-point t)
                     (agent-switch-get-client
                      (completing-read
                       "Client: "
                       (mapcar #'agent-switch-client-id
                               (agent-switch-clients)) nil t))))
         (payload (make-hash-table :test #'equal))
         (profile (agent-switch--make-profile
                   :id "new-profile" :client-id (agent-switch-client-id client)
                   :name "New Profile" :payload payload
                   :ownership 'managed :valid-p t)))
    (agent-switch--ensure-client-idle client)
    (agent-switch--open-profile-form client profile t)))

(defun agent-switch-profile-edit ()
  "Edit the managed Profile at point."
  (interactive)
  (let* ((profile (agent-switch--profile-at-point))
         (client (agent-switch-get-client
                  (agent-switch-profile-client-id profile))))
    (agent-switch--ensure-client-idle client)
    (if (eq (agent-switch-profile-ownership profile) 'external)
        (let* ((adapter (agent-switch-get-adapter
                         (agent-switch-client-adapter-id client)))
               (edit (agent-switch-adapter-callback adapter :edit-profile)))
          (unless edit
            (user-error "External Profiles are read-only; copy it as managed first"))
          (agent-switch--run-client-operation
           client "External Profile edit" (funcall edit client profile nil)))
      (unless (agent-switch-profile-valid-p profile)
        (user-error "Open the damaged JSON file and repair it first"))
      (agent-switch--open-profile-form client profile nil))))

(defun agent-switch--copy-profile (profile id name)
  "Open a managed copy of PROFILE using ID and NAME."
  (let* ((client (agent-switch-get-client
                  (agent-switch-profile-client-id profile)))
         (copy (copy-agent-switch-profile profile)))
    (agent-switch--ensure-client-idle client)
    (setf (agent-switch-profile-id copy) id
          (agent-switch-profile-name copy) name
          (agent-switch-profile-ownership copy) 'managed
          (agent-switch-profile-source copy) nil
          (agent-switch-profile-source-hash copy) nil
          (agent-switch-profile-valid-p copy) t
          (agent-switch-profile-error copy) nil)
    (agent-switch--open-profile-form client copy t)))

(defun agent-switch-profile-duplicate ()
  "Duplicate the Profile at point as a managed Profile."
  (interactive)
  (let* ((profile (agent-switch--profile-at-point))
         (id (read-string "New Profile ID: "
                          (concat (agent-switch-profile-id profile) "-copy"))))
    (agent-switch--copy-profile
     profile id (concat (agent-switch-profile-name profile) " Copy"))))

(defun agent-switch-profile-copy-as-managed ()
  "Copy the external Profile at point as managed."
  (interactive)
  (let ((profile (agent-switch--profile-at-point)))
    (agent-switch--ensure-client-idle
     (agent-switch-get-client (agent-switch-profile-client-id profile)))
    (unless (eq (agent-switch-profile-ownership profile) 'external)
      (user-error "Profile is already managed"))
    (agent-switch--copy-profile
     profile
     (read-string "Managed Profile ID: " (agent-switch-profile-id profile))
     (agent-switch-profile-name profile))))

(defun agent-switch-profile-rename ()
  "Rename the managed Profile display name at point."
  (interactive)
  (let ((profile (agent-switch--profile-at-point)))
    (agent-switch--ensure-client-idle
     (agent-switch-get-client (agent-switch-profile-client-id profile)))
    (unless (eq (agent-switch-profile-ownership profile) 'managed)
      (user-error "External Profiles cannot be renamed"))
    (setf (agent-switch-profile-name profile)
          (read-string "Display name: " (agent-switch-profile-name profile)))
    (agent-switch-save-profile profile)
    (agent-switch-refresh-dashboards)))

(defun agent-switch-profile-change-id ()
  "Change the managed Profile ID using an explicit copy/delete operation."
  (interactive)
  (let* ((profile (agent-switch--profile-at-point))
         (new-id (agent-switch--string-id
                  (read-string "New Profile ID: "
                               (agent-switch-profile-id profile))
                  "profile")))
    (agent-switch--ensure-client-idle
     (agent-switch-get-client (agent-switch-profile-client-id profile)))
    (unless (eq (agent-switch-profile-ownership profile) 'managed)
      (user-error "External Profiles cannot change ID"))
    (unless (yes-or-no-p
             (format "Change Profile ID %s to %s? "
                     (agent-switch-profile-id profile) new-id))
      (user-error "Cancelled"))
    (let ((copy (copy-agent-switch-profile profile)))
      (setf (agent-switch-profile-id copy) new-id
            (agent-switch-profile-source copy) nil
            (agent-switch-profile-source-hash copy) nil)
      (agent-switch-save-profile copy)
      (condition-case error-value
          (agent-switch-delete-profile profile)
        (error
         (signal 'agent-switch-error
                 (list (format "New Profile saved, but old ID could not be removed: %s"
                               (agent-switch--safe-error-message error-value)))))))
    (agent-switch-refresh-dashboards)))

(defun agent-switch-profile-delete ()
  "Delete the managed Profile at point without changing the live Client."
  (interactive)
  (let* ((profile (agent-switch--profile-at-point))
         (client-id (agent-switch-profile-client-id profile))
         (client (agent-switch-get-client client-id))
         (adapter (agent-switch-get-adapter
                   (agent-switch-client-adapter-id client)))
         (current-result (agent-switch--client-current client))
         (current (nth 0 current-result))
         (current-p (and current
                         (condition-case nil
                             (agent-switch-profile-current-p
                              client profile current nil)
                           (error nil)))))
    (agent-switch--ensure-client-idle client)
    (when current-p
      (unless (yes-or-no-p
               "Active Profile; keep live Client unchanged and continue? ")
        (user-error "Cancelled")))
    (unless (yes-or-no-p
             (format "Delete managed Profile %s? "
                     (agent-switch-profile-name profile)))
      (user-error "Cancelled"))
    (if (eq (agent-switch-profile-ownership profile) 'managed)
        (progn
          (agent-switch-delete-profile profile)
          (agent-switch-refresh-dashboards))
      (let ((delete (agent-switch-adapter-callback adapter :delete-profile)))
        (unless delete
          (user-error "External Profiles cannot be deleted"))
        (agent-switch--run-client-operation
         client "External Profile delete"
         (funcall delete client profile nil))))))

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

(defun agent-switch-open-client-config ()
  "Open the Client configuration path at point."
  (interactive)
  (let* ((client (agent-switch--client-at-point))
         (adapter (agent-switch-get-adapter
                   (agent-switch-client-adapter-id client)))
         (paths (agent-switch-adapter-callback adapter :watch-paths)))
    (unless paths (user-error "Adapter does not expose a config path"))
    (let ((result (funcall paths client nil)))
      (when (agent-switch-job-p result)
        (user-error "Config path is still loading"))
      (find-file (car result)))))

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
  [["View"
    ("g" "Refresh" agent-switch-refresh)
    ("RET" "Details" agent-switch-profile-details)
    ("d" "Diagnose" agent-switch-diagnose)
    ("o" "Open client config" agent-switch-open-client-config)
    ("f" "Open Profile JSON" agent-switch-open-profile-file)]
   ["Activate"
    ("s" "Activate Profile" agent-switch-activate-at-point)
    ("l" "Reapply last selected" agent-switch-reapply-last-selected)
    ("A" "Adopt current" agent-switch-adopt-current)
    ("r" "Reset damaged state" agent-switch-reset-state)]
   ["Manage"
    ("a" "Create" agent-switch-profile-create)
    ("e" "Edit" agent-switch-profile-edit)
    ("c" "Copy external" agent-switch-profile-copy-as-managed)
    ("u" "Duplicate" agent-switch-profile-duplicate)
    ("R" "Rename" agent-switch-profile-rename)
    ("I" "Change ID" agent-switch-profile-change-id)
    ("D" "Delete" agent-switch-profile-delete)]
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
    (define-key map (kbd "s") #'agent-switch-activate-at-point)
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
  (evil-set-initial-state 'agent-switch-profile-edit-mode 'normal)
  ;; Only shared structural/action keys are installed in Evil normal state.
  ;; g, n/p, and M-n/M-p intentionally remain native Evil bindings.
  (dolist (binding '(("TAB" . agent-switch-toggle-section)
                     ("<tab>" . agent-switch-toggle-section)
                     ("<backtab>" . agent-switch-cycle-sections)
                     ("<S-tab>" . agent-switch-cycle-sections)
                     ("RET" . agent-switch-return)
                     ("s" . agent-switch-activate-at-point)
                     ("?" . agent-switch-menu)
                     ("q" . quit-window)))
    (evil-define-key* 'normal agent-switch-mode-map
      (kbd (car binding)) (cdr binding)))
  (evil-define-key* 'normal agent-switch-profile-edit-mode-map
    (kbd "C-c C-c") #'agent-switch-profile-edit-save
    (kbd "C-c C-k") #'agent-switch-profile-edit-cancel
    (kbd "TAB") #'agent-switch-profile-edit-next-field
    (kbd "<tab>") #'agent-switch-profile-edit-next-field
    (kbd "<backtab>") #'agent-switch-profile-edit-previous-field
    (kbd "<S-tab>") #'agent-switch-profile-edit-previous-field))

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
