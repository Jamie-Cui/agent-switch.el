;;; agent-switch-test.el --- Tests for agent-switch.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'json)
(require 'agent-switch)

(defmacro agent-switch-test--with-root (root &rest body)
  "Run BODY with isolated registries and configuration below ROOT."
  (declare (indent 1))
  `(let* ((,root (make-temp-file "agent-switch-test-" t))
          (agent-switch-directory (expand-file-name "data/" ,root))
          (agent-switch-claude-config-directory
           (expand-file-name ".claude/" ,root))
          (agent-switch-codex-home (expand-file-name ".codex/" ,root))
          (agent-switch-opencode-config-file
           (expand-file-name "opencode/opencode.jsonc" ,root))
          (agent-switch-confirm-canonical-rewrite nil)
          (agent-switch--adapters (make-hash-table :test #'equal))
          (agent-switch--clients (make-hash-table :test #'equal))
          (agent-switch--client-order nil)
          (agent-switch--external-profiles (make-hash-table :test #'equal))
          (agent-switch--discovery-cache (make-hash-table :test #'equal))
          (agent-switch--running-jobs (make-hash-table :test #'equal)))
     (unwind-protect
         (progn
           (agent-switch-register-builtins)
           ,@body)
       (ignore-errors (delete-directory ,root t)))))

(defun agent-switch-test--hash (&rest pairs)
  "Return an equal-tested hash table from PAIRS."
  (let ((table (make-hash-table :test #'equal)))
    (while pairs
      (puthash (pop pairs) (pop pairs) table))
    table))

(defun agent-switch-test--profile (client-id id payload &optional name)
  "Return a new managed Profile for CLIENT-ID, ID, and PAYLOAD."
  (agent-switch--make-profile
   :id id :client-id client-id :name (or name id)
   :payload payload :ownership 'managed :valid-p t))

(defun agent-switch-test--run-job (job)
  "Run synchronous JOB and return its value or signal its error."
  (let (settled value error-value)
    (agent-switch-job-start
     job
     (lambda (result) (setq settled t value result))
     (lambda (error-result) (setq settled t error-value error-result)))
    (unless settled
      (error "Test Job did not settle synchronously"))
    (when error-value
      (signal (car error-value) (cdr error-value)))
    value))

(defun agent-switch-test--read-json-file (path)
  "Read JSON object from PATH."
  (agent-switch-parse-json
   (with-temp-buffer
     (insert-file-contents path)
     (buffer-string))))

(ert-deftest agent-switch-identifiers-reject-path-traversal ()
  (should-error (agent-switch--string-id "../escape" "profile")
                :type 'agent-switch-validation-error)
  (should (equal (agent-switch--string-id 'openai-main "profile")
                 "openai-main")))

(ert-deftest agent-switch-registry-supports-elisp-extensions ()
  (agent-switch-test--with-root root
    (agent-switch-define-adapter demo
      :name "Demo"
      :current (lambda (_client _context)
                 (agent-switch-test--hash "model" "one"))
      :activate (lambda (_client _profile _context) t))
    (agent-switch-register-client 'demo-client :adapter 'demo :name "Demo Client")
    (agent-switch-register-profile
     'demo-client 'from-elisp :name "From Elisp"
     :payload (agent-switch-test--hash "model" "one"))
    (let ((profile (car (agent-switch-external-profiles "demo-client"))))
      (should (eq (agent-switch-profile-ownership profile) 'external))
      (should (equal (agent-switch-profile-id profile) "from-elisp")))))

(ert-deftest agent-switch-adapter-requires-current-and-activate ()
  (agent-switch-test--with-root root
    (should-error
     (agent-switch-register-adapter 'broken :current #'ignore)
     :type 'agent-switch-validation-error)))

(ert-deftest agent-switch-profile-roundtrip-uses-versioned-json ()
  (agent-switch-test--with-root root
    (let* ((profile (agent-switch-test--profile
                     "claude" "work"
                     (agent-switch-test--hash
                      "env" (agent-switch-test--hash
                             "ANTHROPIC_BASE_URL" "https://example.test"))
                     "Work"))
           (saved (agent-switch-save-profile profile))
           (loaded (car (agent-switch-load-managed-profiles "claude")))
           (json (agent-switch-test--read-json-file
                  (agent-switch-profile-source saved))))
      (should (equal (gethash "schema_version" json) 1))
      (should (equal (agent-switch-profile-id loaded) "work"))
      (should (eq (agent-switch-profile-ownership loaded) 'managed))
      (should (stringp (agent-switch-profile-source-hash loaded))))))

(ert-deftest agent-switch-plaintext-secrets-are-rejected ()
  (agent-switch-test--with-root root
    (let ((profile (agent-switch-test--profile
                    "claude" "unsafe"
                    (agent-switch-test--hash
                     "env" (agent-switch-test--hash
                            "ANTHROPIC_AUTH_TOKEN" "secret-value")))))
      (should-error (agent-switch-save-profile profile)
                    :type 'agent-switch-validation-error))))

(ert-deftest agent-switch-secret-references-resolve-without-persistence ()
  (agent-switch-test--with-root root
    (let* ((reference (agent-switch-test--hash
                       "source" "env" "name" "AGENT_SWITCH_TEST_TOKEN"))
           (profile (agent-switch-test--profile
                     "claude" "safe"
                     (agent-switch-test--hash
                      "env" (agent-switch-test--hash
                             "ANTHROPIC_AUTH_TOKEN" reference)))))
      (setenv "AGENT_SWITCH_TEST_TOKEN" "resolved-secret")
      (unwind-protect
          (progn
            (agent-switch-save-profile profile)
            (let* ((result (agent-switch-resolve-profile-secrets profile))
                   (resolved (car result)))
              (should (equal
                       (agent-switch-json-get-in
                        (agent-switch-profile-payload resolved)
                        '("env" "ANTHROPIC_AUTH_TOKEN"))
                       "resolved-secret"))
              (should-not
               (string-match-p
                "resolved-secret"
                (agent-switch--read-file-text
                 (agent-switch-profile-source profile))))))
        (setenv "AGENT_SWITCH_TEST_TOKEN" nil)))))

(ert-deftest agent-switch-auth-source-reference-resolves-function-secret ()
  (let ((reference (agent-switch-test--hash
                    "source" "auth-source"
                    "host" "api.example.test"
                    "user" "agent")))
    (cl-letf (((symbol-function 'auth-source-search)
               (lambda (&rest _arguments)
                 (list (list :secret (lambda () "auth-secret"))))))
      (should (equal (agent-switch--resolve-secret-reference reference)
                     "auth-secret")))))

(ert-deftest agent-switch-corrupt-profile-is-isolated ()
  (agent-switch-test--with-root root
    (let ((directory (agent-switch-profiles-directory "claude")))
      (make-directory directory t)
      (write-region "{broken" nil (expand-file-name "broken.json" directory))
      (agent-switch-save-profile
       (agent-switch-test--profile
        "claude" "valid"
        (agent-switch-test--hash
         "env" (agent-switch-test--hash
                "ANTHROPIC_BASE_URL" "https://example.test"))))
      (let ((profiles (agent-switch-load-managed-profiles "claude")))
        (should (= (length profiles) 2))
        (should-not (agent-switch-profile-valid-p
                     (cl-find "broken" profiles
                              :key #'agent-switch-profile-id :test #'equal)))
        (should (agent-switch-profile-valid-p
                 (cl-find "valid" profiles
                          :key #'agent-switch-profile-id :test #'equal)))))))

(ert-deftest agent-switch-damaged-state-is-read-only-until-reset ()
  (agent-switch-test--with-root root
    (make-directory (agent-switch--directory) t)
    (write-region "not-json" nil (agent-switch-state-path))
    (should (agent-switch-state-record-error (agent-switch-read-state)))
    (should-error (agent-switch-state-set-last-selected "claude" "work")
                  :type 'agent-switch-validation-error)
    (let ((backup (agent-switch-reset-state)))
      (should (file-exists-p backup))
      (should-not (agent-switch-state-record-error (agent-switch-read-state))))))

(ert-deftest agent-switch-optimistic-write-preserves-external-change ()
  (agent-switch-test--with-root root
    (let* ((path (expand-file-name "conflict.json" root))
           (snapshot (agent-switch-capture-file path)))
      (write-region "external" nil path)
      (should-error
       (agent-switch-write-text-atomic
        path "ours" (agent-switch-file-state-hash snapshot))
       :type 'agent-switch-conflict)
      (should (equal (agent-switch--read-file-text path) "external")))))

(ert-deftest agent-switch-profile-order-lives-in-state ()
  (agent-switch-test--with-root root
    (dolist (id '("a" "b"))
      (agent-switch-save-profile
       (agent-switch-test--profile
        "claude" id
        (agent-switch-test--hash
         "env" (agent-switch-test--hash
                "ANTHROPIC_BASE_URL" (concat "https://" id))))))
    (agent-switch-register-profile
     'claude 'external :name "External"
     :payload (agent-switch-test--hash
               "env" (agent-switch-test--hash
                      "ANTHROPIC_BASE_URL" "https://external")))
    (agent-switch-state-set-profile-order
     "claude" '("external" "b" "a" "deleted"))
    (should (equal (mapcar #'agent-switch-profile-id
                           (agent-switch-profiles "claude"))
                   '("external" "b" "a")))))

(ert-deftest agent-switch-activation-rolls-back-on-verification-failure ()
  (agent-switch-test--with-root root
    (let ((state (agent-switch-test--hash "value" "old")))
      (agent-switch-define-adapter rollback-demo
        :current (lambda (_client _context) state)
        :activate (lambda (_client _profile _context)
                    (setq state (agent-switch-test--hash "value" "wrong")))
        :snapshot (lambda (_client _profile _context)
                    (agent-switch-json-copy state))
        :rollback (lambda (_client snapshot _context)
                    (setq state snapshot)))
      (let* ((client (agent-switch-register-client
                      'rollback-demo :adapter 'rollback-demo))
             (profile (agent-switch-test--profile
                       "rollback-demo" "new"
                       (agent-switch-test--hash "value" "expected"))))
        (should-error
         (agent-switch-test--run-job
          (agent-switch-activation-job client profile))
         :type 'agent-switch-error)
        (should (equal (gethash "value" state) "old"))))))

(ert-deftest agent-switch-async-discovery-is-supported ()
  (agent-switch-test--with-root root
    (let ((discovered
           (agent-switch--make-profile
            :id "remote" :client-id "async-client" :name "Remote"
            :payload (agent-switch-test--hash "value" "one")
            :ownership 'external :source 'adapter :valid-p t)))
      (agent-switch-define-adapter async-adapter
        :current (lambda (_client _context)
                   (agent-switch-test--hash "value" "one"))
        :activate (lambda (_client _profile _context) t)
        :discover
        (lambda (_client _context)
          (agent-switch-job-create
           :starter (lambda (resolve _reject)
                      (funcall resolve (list discovered))))))
      (agent-switch-register-client 'async-client :adapter 'async-adapter)
      (should-not (agent-switch-profiles "async-client"))
      (should (equal (mapcar #'agent-switch-profile-id
                             (agent-switch-profiles "async-client"))
                     '("remote"))))))

(ert-deftest agent-switch-dashboard-initializes-current-config-as-default ()
  (agent-switch-test--with-root root
    (let ((activations 0)
          (captures 0)
          (current (agent-switch-test--hash "model" "live")))
      (agent-switch-define-adapter bootstrap-adapter
        :current (lambda (_client _context) current)
        :activate (lambda (_client _profile _context)
                    (setq activations (1+ activations)))
        :validate (lambda (_client profile _context)
                    (unless (equal (gethash "model"
                                            (agent-switch-profile-payload profile))
                                   "live")
                      (signal 'agent-switch-validation-error
                              '("Unexpected bootstrap payload")))
                    t)
        :capture-current (lambda (_client value _context)
                           (setq captures (1+ captures))
                           (agent-switch-json-copy value)))
      (let ((client (agent-switch-register-client
                     'bootstrap-client :adapter 'bootstrap-adapter)))
        (with-temp-buffer
          (agent-switch-mode)
          (let* ((view (agent-switch--client-view client))
                 (profile (car (agent-switch-client-view-profiles view))))
            (should-not (agent-switch-client-view-error view))
            (should (equal (gethash "model"
                                    (agent-switch-client-view-current view))
                           "live"))
            (should profile)
            (should (equal (agent-switch-profile-id profile) "default"))
            (should (equal (agent-switch-profile-name profile) "Default"))
            (should (eq (agent-switch-profile-ownership profile) 'managed))
            (should (file-exists-p (agent-switch-profile-source profile)))
            (should (eq profile
                        (agent-switch-client-view-current-profile view)))
            (should (equal (agent-switch-client-view-last-selected view)
                           "default"))
            (should (equal (agent-switch-state-last-selected
                            "bootstrap-client")
                           "default"))
            (should (= captures 1))
            (should (= activations 0))))))))

(ert-deftest agent-switch-dashboard-does-not-bootstrap-over-existing-profile ()
  (agent-switch-test--with-root root
    (let ((captures 0)
          (current (agent-switch-test--hash "model" "existing")))
      (agent-switch-define-adapter existing-adapter
        :current (lambda (_client _context) current)
        :activate (lambda (_client _profile _context) t)
        :capture-current (lambda (_client value _context)
                           (setq captures (1+ captures))
                           (agent-switch-json-copy value)))
      (let* ((client (agent-switch-register-client
                      'existing-client :adapter 'existing-adapter))
             (existing (agent-switch-save-profile
                        (agent-switch-test--profile
                         "existing-client" "existing" current "Existing"))))
        (with-temp-buffer
          (agent-switch-mode)
          (let ((view (agent-switch--client-view client)))
            (should (equal
                     (mapcar #'agent-switch-profile-id
                             (agent-switch-client-view-profiles view))
                     (list (agent-switch-profile-id existing))))
            (should (= captures 0))
            (should-not (file-exists-p
                         (agent-switch-profile-path
                          "existing-client" "default")))))))))

(ert-deftest agent-switch-dashboard-requires-setup-when-current-has-secrets ()
  (agent-switch-test--with-root root
    (let ((captures 0)
          (current
           (agent-switch-test--hash
            "env" (agent-switch-test--hash
                   "BASE_URL" "https://secret.test"
                   "API_TOKEN" (agent-switch--secret-marker "secret")))))
      (agent-switch-define-adapter secret-bootstrap-adapter
        :current (lambda (_client _context) current)
        :activate (lambda (_client _profile _context) t)
        :capture-current (lambda (_client value _context)
                           (setq captures (1+ captures))
                           (agent-switch-json-copy value)))
      (let ((client (agent-switch-register-client
                     'secret-bootstrap-client
                     :adapter 'secret-bootstrap-adapter)))
        (with-temp-buffer
          (agent-switch-mode)
          (let ((view (agent-switch--client-view client)))
            (should-not (agent-switch-client-view-profiles view))
            (should (eq (agent-switch-client-view-bootstrap-status view)
                        'secret-required))
            (should (string-match-p
                     "default setup required"
                     (substring-no-properties
                      (agent-switch--client-status view))))
            (should (= captures 0))
            (should-not (file-exists-p
                         (agent-switch-profile-path
                          "secret-bootstrap-client" "default")))))))))

(ert-deftest agent-switch-dashboard-does-not-bootstrap-with-damaged-state ()
  (agent-switch-test--with-root root
    (make-directory (agent-switch--directory) t)
    (write-region "not-json" nil (agent-switch-state-path))
    (let ((captures 0))
      (agent-switch-define-adapter damaged-state-adapter
        :current (lambda (_client _context)
                   (agent-switch-test--hash "model" "live"))
        :activate (lambda (_client _profile _context) t)
        :capture-current (lambda (_client value _context)
                           (setq captures (1+ captures))
                           (agent-switch-json-copy value)))
      (let ((client (agent-switch-register-client
                     'damaged-state-client :adapter 'damaged-state-adapter)))
        (with-temp-buffer
          (agent-switch-mode)
          (let ((view (agent-switch--client-view client)))
            (should-not (agent-switch-client-view-profiles view))
            (should (= captures 0))
            (should-not (file-exists-p
                         (agent-switch-profile-path
                          "damaged-state-client" "default")))))))))

(ert-deftest agent-switch-dashboard-reports-invalid-bootstrap-capture ()
  (agent-switch-test--with-root root
    (agent-switch-define-adapter invalid-capture-adapter
      :current (lambda (_client _context)
                 (agent-switch-test--hash "model" "live"))
      :activate (lambda (_client _profile _context) t)
      :capture-current (lambda (_client _value _context) nil))
    (let ((client (agent-switch-register-client
                   'invalid-capture-client :adapter 'invalid-capture-adapter)))
      (with-temp-buffer
        (agent-switch-mode)
        (let ((view (agent-switch--client-view client)))
          (should (string-match-p
                   "capture-current must return a Profile payload object"
                   (agent-switch-client-view-error view)))
          (should-not (file-exists-p
                       (agent-switch-profile-path
                        "invalid-capture-client" "default"))))))))

(ert-deftest agent-switch-dashboard-waits-for-discovered-profiles-before-bootstrap ()
  (agent-switch-test--with-root root
    (let ((captures 0)
          (discovered
           (agent-switch--make-profile
            :id "remote" :client-id "discovered-client" :name "Remote"
            :payload (agent-switch-test--hash "model" "remote")
            :ownership 'external :source 'adapter :valid-p t)))
      (agent-switch-define-adapter discovered-adapter
        :current (lambda (_client _context)
                   (agent-switch-test--hash "model" "remote"))
        :activate (lambda (_client _profile _context) t)
        :capture-current (lambda (_client value _context)
                           (setq captures (1+ captures))
                           (agent-switch-json-copy value))
        :discover (lambda (_client _context)
                    (agent-switch-job-create
                     :starter (lambda (resolve _reject)
                                (funcall resolve (list discovered))))))
      (let ((client (agent-switch-register-client
                     'discovered-client :adapter 'discovered-adapter)))
        (with-temp-buffer
          (agent-switch-mode)
          (let ((view (agent-switch--client-view client)))
            (should (equal
                     (mapcar #'agent-switch-profile-id
                             (agent-switch-client-view-profiles view))
                     '("remote")))
            (should (= captures 0))
            (should-not (file-exists-p
                         (agent-switch-profile-path
                          "discovered-client" "default")))))))))

(ert-deftest agent-switch-dashboard-does-not-bootstrap-after-discovery-error ()
  (agent-switch-test--with-root root
    (let ((captures 0))
      (agent-switch-define-adapter failed-discovery-adapter
        :current (lambda (_client _context)
                   (agent-switch-test--hash "model" "live"))
        :activate (lambda (_client _profile _context) t)
        :capture-current (lambda (_client value _context)
                           (setq captures (1+ captures))
                           (agent-switch-json-copy value))
        :discover (lambda (_client _context)
                    (agent-switch-job-create
                     :starter (lambda (_resolve reject)
                                (funcall reject '(error "Discovery failed"))))))
      (let ((client (agent-switch-register-client
                     'failed-discovery-client
                     :adapter 'failed-discovery-adapter)))
        (with-temp-buffer
          (agent-switch-mode)
          (let ((view (agent-switch--client-view client)))
            (should (string-match-p
                     "Discovery failed"
                     (agent-switch-client-view-error view)))
            (should (= captures 0))
            (should-not (file-exists-p
                         (agent-switch-profile-path
                          "failed-discovery-client" "default")))))))))

(ert-deftest agent-switch-claude-patches-owned-env-only ()
  (agent-switch-test--with-root root
    (make-directory agent-switch-claude-config-directory t)
    (let* ((path (agent-switch--claude-settings-path))
           (settings
            (agent-switch-test--hash
             "permissions" (agent-switch-test--hash "allow" ["Read"])
             "env" (agent-switch-test--hash
                    "OTHER" "keep"
                    "ANTHROPIC_MODEL" "old")))
           (reference (agent-switch-test--hash
                       "source" "env" "name" "AGENT_SWITCH_CLAUDE_TOKEN"))
           (payload (agent-switch-test--hash
                     "env" (agent-switch-test--hash
                            "ANTHROPIC_BASE_URL" "https://relay.test"
                            "ANTHROPIC_AUTH_TOKEN" reference
                            "ANTHROPIC_MODEL" "new")))
           (profile (agent-switch-test--profile
                     "claude" "relay" payload "Relay"))
           (client (agent-switch-get-client "claude")))
      (write-region (agent-switch-json-serialize settings) nil path)
      (setenv "AGENT_SWITCH_CLAUDE_TOKEN" "live-secret")
      (unwind-protect
          (progn
            (agent-switch-test--run-job
             (agent-switch-activation-job client profile))
            (let* ((written (agent-switch-test--read-json-file path))
                   (env (gethash "env" written)))
              (should (hash-table-p (gethash "permissions" written)))
              (should (equal (gethash "OTHER" env) "keep"))
              (should (equal (gethash "ANTHROPIC_MODEL" env) "new"))
              (should (equal (gethash "ANTHROPIC_AUTH_TOKEN" env)
                             "live-secret"))
              (should (cl-some
                       (lambda (file)
                         (string-match-p "agent-switch\\.bak" file))
                       (directory-files agent-switch-claude-config-directory))))
            (should-not
             (string-match-p
              "live-secret"
              (format "%S" (agent-switch--claude-current client nil)))))
        (setenv "AGENT_SWITCH_CLAUDE_TOKEN" nil)))))

(ert-deftest agent-switch-codex-structurally-preserves-unowned-settings ()
  (agent-switch-test--with-root root
    (make-directory agent-switch-codex-home t)
    (let* ((path (agent-switch--codex-config-path))
           (old (concat
                 "model = \"old/model\"\n"
                 "model_provider = \"old\"\n"
                 "sandbox_mode = \"workspace-write\"\n"
                 "developer_instructions = \"Preserve 中文 text\"\n\n"
                 "[model_providers.old]\n"
                 "base_url = \"https://old.test\"\n\n"
                 "[mcp_servers.demo]\n"
                 "command = \"demo\"\n"))
           (provider (agent-switch-test--hash
                      "base_url" "https://new.test"
                      "env_key" "NEW_API_KEY"))
           (payload (agent-switch-test--hash
                     "provider-id" "new"
                     "model" "new/model"
                     "small-model" "new/small"
                     "provider" provider))
           (profile (agent-switch-test--profile "codex" "new" payload))
           (client (agent-switch-get-client "codex")))
      (write-region old nil path)
      (agent-switch-test--run-job
       (agent-switch-activation-job client profile))
      (let* ((config (agent-switch--read-toml-file path))
             (providers (agent-switch--alist-get "model_providers" config))
             (new-provider (agent-switch--alist-get "new" providers)))
        (should (equal (agent-switch--alist-get "sandbox_mode" config)
                       "workspace-write"))
        (should (equal (agent-switch--alist-get "developer_instructions" config)
                       "Preserve 中文 text"))
        (should (agent-switch--alist-get "mcp_servers" config))
        (should (agent-switch--alist-get "old" providers))
        (should (equal (agent-switch--alist-get "base_url" new-provider)
                       "https://new.test"))
        (should (equal (agent-switch--alist-get "model" config) "new/model"))))))

(ert-deftest agent-switch-codex-empty-provider-roundtrips-as-current ()
  (agent-switch-test--with-root root
    (make-directory agent-switch-codex-home t)
    (write-region
     (concat "model = \"gpt-test\"\n"
             "model_provider = \"openai\"\n\n"
             "[model_providers.other]\n"
             "base_url = \"https://other.test\"\n")
     nil (agent-switch--codex-config-path))
    (let* ((client (agent-switch-get-client "codex"))
           (current (agent-switch--codex-current client nil))
           (provider (gethash "provider" current))
           (profile (agent-switch-test--profile
                     "codex" "default"
                     (agent-switch--capture-current client current nil)
                     "Default")))
      (should (hash-table-p provider))
      (should (= (hash-table-count provider) 0))
      (agent-switch-save-profile profile)
      (let ((loaded (agent-switch-find-profile "codex" "default")))
        (should (agent-switch--codex-profile-current-p
                 client loaded current nil))
        (agent-switch-test--run-job
         (agent-switch-activation-job client loaded))))))

(ert-deftest agent-switch-opencode-jsonc-patch-preserves-other-config ()
  (agent-switch-test--with-root root
    (make-directory (file-name-directory agent-switch-opencode-config-file) t)
    (let* ((path (agent-switch--opencode-config-path))
           (project-file (expand-file-name "project/opencode.json" root))
           (payload (agent-switch-test--hash
                     "provider-id" "relay"
                     "model" "relay/large"
                     "small-model" "relay/small"
                     "provider" (agent-switch-test--hash
                                 "npm" "@ai-sdk/openai-compatible"
                                 "options" (agent-switch-test--hash
                                            "baseURL" "https://relay.test"))))
           (profile (agent-switch-test--profile
                     "opencode-global" "relay" payload))
           (client (agent-switch-get-client "opencode-global")))
      (write-region
       (concat "{\n  // keep semantics\n"
               "  \"model\": \"other/old\",\n"
               "  \"permission\": {\"bash\": \"ask\"},\n"
               "  \"provider\": {\"other\": {\"npm\": \"pkg\"}},\n}\n")
       nil path)
      (make-directory (file-name-directory project-file) t)
      (write-region "{\"model\":\"project/model\"}\n" nil project-file)
      (agent-switch-test--run-job
       (agent-switch-activation-job client profile))
      (let* ((written (agent-switch-test--read-json-file path))
             (providers (gethash "provider" written)))
        (should (equal (gethash "bash" (gethash "permission" written)) "ask"))
        (should (gethash "other" providers))
        (should (equal (gethash "model" written) "relay/large"))
        (should (equal (agent-switch--read-file-text project-file)
                       "{\"model\":\"project/model\"}\n"))))))

(ert-deftest agent-switch-gptel-only-changes-global-defaults ()
  (agent-switch-test--with-root root
    (require 'gptel)
    (require 'gptel-openai)
    (let* ((backend-name "Agent Switch Test")
           (backend (gptel-make-openai backend-name
                      :models '(agent-switch-test-model)))
           (model 'agent-switch-test-model)
           (old-backend (default-toplevel-value 'gptel-backend))
           (old-model (default-toplevel-value 'gptel-model))
           (profile (agent-switch-test--profile
                     "gptel-default" "default"
                     (agent-switch-test--hash
                      "backend-name" backend-name
                      "model" (symbol-name model))))
           (client (agent-switch-get-client "gptel-default")))
      (unwind-protect
          (with-temp-buffer
            (setq-local gptel-backend :buffer-backend)
            (setq-local gptel-model :buffer-model)
            (agent-switch-test--run-job
             (agent-switch-activation-job client profile))
            (should (eq gptel-backend :buffer-backend))
            (should (eq gptel-model :buffer-model))
            (should (eq (default-toplevel-value 'gptel-backend) backend))
            (should (eq (default-toplevel-value 'gptel-model) model)))
        (set-default-toplevel-value 'gptel-backend old-backend)
        (set-default-toplevel-value 'gptel-model old-model)))))

(ert-deftest agent-switch-gptel-runtime-watcher-cleans-up ()
  (agent-switch-test--with-root root
    (require 'gptel)
    (let ((calls 0)
          (old-model (default-toplevel-value 'gptel-model))
          cleanup)
      (unwind-protect
          (progn
            (setq cleanup
                  (agent-switch--gptel-watch-setup
                   (agent-switch-get-client "gptel-default")
                   (lambda () (setq calls (1+ calls)))))
            (set-default-toplevel-value 'gptel-model 'watch-test)
            (should (> calls 0))
            (funcall cleanup)
            (setq cleanup nil calls 0)
            (set-default-toplevel-value 'gptel-model 'watch-test-2)
            (should (= calls 0)))
        (when cleanup (funcall cleanup))
        (set-default-toplevel-value 'gptel-model old-model)))))

(ert-deftest agent-switch-dashboard-uses-internal-sections ()
  (agent-switch-test--with-root root
    (agent-switch-save-profile
     (agent-switch-test--profile
      "claude" "work"
      (agent-switch-test--hash
       "env" (agent-switch-test--hash
              "ANTHROPIC_BASE_URL" "https://example.test"))
      "Work"))
    (with-temp-buffer
      (agent-switch-mode)
      (agent-switch-refresh)
      (should (derived-mode-p 'special-mode))
      (should-not (derived-mode-p 'tabulated-list-mode))
      (should-not (gethash "status" agent-switch--sections))
      (goto-char (point-min))
      (should (looking-at "Data:"))
      (should-not (agent-switch--section-at-point t))
      (let* ((id "client/claude/profile/work")
             (section (gethash id agent-switch--sections)))
        (should section)
        (should-not (agent-switch-section-expanded-p section))
        (goto-char (agent-switch-section-start section))
        (agent-switch-toggle-section)
        (setq section (gethash id agent-switch--sections))
        (should (agent-switch-section-expanded-p section))
        (should (equal (agent-switch--point-section-id) id))))))

(ert-deftest agent-switch-dashboard-has-no-blank-lines-between-sections ()
  (agent-switch-test--with-root root
    (with-temp-buffer
      (agent-switch-mode)
      (agent-switch-refresh)
      (let ((clients (agent-switch--visible-sections 'client)))
        (should clients)
        (dolist (section clients)
          (let ((end (agent-switch-section-end section)))
            (should-not (equal (buffer-substring-no-properties
                                (max (point-min) (- end 2)) end)
                               "\n\n"))))
        (should (= (agent-switch-section-end (car (last clients)))
                   (point-max)))))))

(ert-deftest agent-switch-client-headings-use-inline-status ()
  (agent-switch-test--with-root root
    (with-temp-buffer
      (agent-switch-mode)
      (agent-switch-refresh)
      (dolist (section (agent-switch--visible-sections 'client))
        (let* ((client (agent-switch-section-value section))
               (prefix (concat (agent-switch-client-name client) " ("))
               (start (agent-switch-section-start section)))
          (goto-char start)
          (should (string-prefix-p
                   prefix
                   (buffer-substring-no-properties
                    start (line-end-position))))
          (let ((face (get-text-property start 'face)))
            (should (if (listp face)
                        (memq 'agent-switch-key face)
                      (eq face 'agent-switch-key))))
          (should (string-suffix-p
                   ")"
                   (buffer-substring-no-properties
                    start (line-end-position)))))))))

(ert-deftest agent-switch-status-preamble-omits-last-operation-errors ()
  (agent-switch-test--with-root root
    (with-temp-buffer
      (agent-switch-mode)
      (let ((agent-switch--last-error "Verification failed")
            (inhibit-read-only t))
        (agent-switch--insert-status))
      (should-not (string-match-p "Last operation" (buffer-string)))
      (should-not (string-match-p "Verification failed" (buffer-string)))
      (goto-char (point-min))
      (search-forward "Data: ")
      (should (eq (get-text-property (point) 'face) 'default)))))

(ert-deftest agent-switch-activation-failure-is-message-only ()
  (agent-switch-test--with-root root
    (agent-switch-define-adapter message-only-adapter
      :current (lambda (_client _context) nil)
      :activate (lambda (_client _profile _context) t))
    (let* ((client (agent-switch-register-client
                    'message-only-client :adapter 'message-only-adapter))
           (profile (agent-switch-test--profile
                     "message-only-client" "failure"
                     (agent-switch-test--hash "model" "unused")))
           logged)
      (with-temp-buffer
        (agent-switch-mode)
        (agent-switch-refresh)
        (cl-letf (((symbol-function 'agent-switch-state-unprotected-confirmed-p)
                   (lambda (_adapter-id) t))
                  ((symbol-function 'agent-switch-activation-job)
                   (lambda (_client _profile &optional _interactivep)
                     (agent-switch-job-create
                      :starter
                      (lambda (_resolve reject)
                        (funcall reject
                                 '(agent-switch-error
                                   "Verification failed"))))))
                  ((symbol-function 'message)
                   (lambda (format-string &rest arguments)
                     (setq logged (apply #'format format-string arguments)))))
          (agent-switch--activate-profile client profile))
        (should (string-match-p "Verification failed" logged))
        (should-not (string-match-p "Last operation" (buffer-string)))
        (should-not (string-match-p "Verification failed"
                                    (buffer-string)))))))

(ert-deftest agent-switch-dashboard-highlights-innermost-section-at-point ()
  (agent-switch-test--with-root root
    (agent-switch-save-profile
     (agent-switch-test--profile
      "claude" "highlighted"
      (agent-switch-test--hash
       "env" (agent-switch-test--hash
              "ANTHROPIC_BASE_URL" "https://highlight.test"))))
    (with-temp-buffer
      (agent-switch-mode)
      (agent-switch--set-visible "client/claude/profile/highlighted" t)
      (agent-switch-refresh)
      (should (eq (face-attribute 'agent-switch-section-highlight
                                  :inherit nil t)
                  'secondary-selection))
      (let ((section (gethash "client/claude/profile/highlighted"
                              agent-switch--sections)))
        (goto-char (1- (agent-switch-section-end section)))
        (should (eq (agent-switch--section-at-point) section))
        (agent-switch--update-section-highlight)
        (should (memq #'agent-switch--update-section-highlight
                      post-command-hook))
        (should (overlayp agent-switch--section-highlight-overlay))
        (should (= (overlay-start agent-switch--section-highlight-overlay)
                   (agent-switch-section-start section)))
        (should (= (overlay-end agent-switch--section-highlight-overlay)
                   (agent-switch-section-end section)))
        (should (eq (overlay-get agent-switch--section-highlight-overlay
                                 'font-lock-face)
                    'agent-switch-section-highlight))
        (goto-char (point-min))
        (agent-switch--update-section-highlight)
        (should-not agent-switch--section-highlight-overlay)))))

(ert-deftest agent-switch-global-cycle-expands-profiles-below-hidden-client ()
  (agent-switch-test--with-root root
    (agent-switch-save-profile
     (agent-switch-test--profile
      "claude" "hidden"
      (agent-switch-test--hash
       "env" (agent-switch-test--hash
              "ANTHROPIC_BASE_URL" "https://hidden.test"))))
    (with-temp-buffer
      (agent-switch-mode)
      (agent-switch-refresh)
      (agent-switch--set-visible "client/claude" nil)
      (agent-switch-refresh t)
      (should-not (gethash "client/claude/profile/hidden"
                           agent-switch--sections))
      (setq agent-switch--cycle-state 1)
      (agent-switch-cycle-sections)
      (let ((profile-section
             (gethash "client/claude/profile/hidden"
                      agent-switch--sections)))
        (should profile-section)
        (should (agent-switch-section-expanded-p profile-section))))))

(ert-deftest agent-switch-dashboard-keymap-separates-shared-and-non-evil-keys ()
  (should (eq (lookup-key agent-switch-mode-map (kbd "TAB"))
              #'agent-switch-toggle-section))
  (should (eq (lookup-key agent-switch-mode-map (kbd "g"))
              #'agent-switch-refresh))
  (should (eq (lookup-key agent-switch-mode-map (kbd "n"))
              #'agent-switch-next-section))
  (should (eq (lookup-key agent-switch-mode-map (kbd "M-n"))
              #'agent-switch-next-sibling-section))
  (should (eq (lookup-key agent-switch-mode-map (kbd "M-p"))
              #'agent-switch-previous-sibling-section))
  (should-not (lookup-key agent-switch-mode-map (kbd "s"))))

(ert-deftest agent-switch-menu-does-not-expose-import-current ()
  (should-error (transient-get-suffix 'agent-switch-menu "i"))
  (should (commandp #'agent-switch-import-current)))

(ert-deftest agent-switch-mode-recovers-old-nil-operation-registry ()
  (let ((original (default-value 'agent-switch--running-jobs)))
    (unwind-protect
        (progn
          (set-default 'agent-switch--running-jobs nil)
          (with-temp-buffer
            (agent-switch-mode)
            (should (hash-table-p agent-switch--running-jobs))
            (should (hash-table-p
                     (default-value 'agent-switch--running-jobs)))))
      (set-default 'agent-switch--running-jobs original))))

(ert-deftest agent-switch-display-width-handles-wide-profile-names ()
  (let ((cell (agent-switch--display-width "模型提供商名称" 12)))
    (should (= (string-width cell) 12))))

(ert-deftest agent-switch-profile-details-never-render-payload ()
  (agent-switch-test--with-root root
    (let* ((profile (agent-switch-test--profile
                     "claude" "safe"
                     (agent-switch-test--hash
                      "env" (agent-switch-test--hash
                             "ANTHROPIC_AUTH_TOKEN"
                             (agent-switch-test--hash
                              "source" "env" "name" "TOKEN")))))
           (client (agent-switch-get-client "claude")))
      (with-temp-buffer
        (agent-switch--insert-profile-details client profile)
        (should-not (string-match-p "ANTHROPIC_AUTH_TOKEN" (buffer-string)))
        (should-not (string-match-p "TOKEN" (buffer-string)))))))

(ert-deftest agent-switch-profile-file-workflow-saves-and-opens-json ()
  (agent-switch-test--with-root root
    (let* ((client (agent-switch-get-client "claude"))
           (payload (agent-switch-test--hash
                     "env" (agent-switch-test--hash
                            "ANTHROPIC_BASE_URL" "https://file.test")))
           opened profile)
      (cl-letf (((symbol-function 'agent-switch--random-profile-id)
                 (lambda (_client-id) "p-1234abcd"))
                ((symbol-function 'find-file)
                 (lambda (path) (setq opened path))))
        (setq profile
              (agent-switch--save-new-profile client "File Profile" payload)))
      (should (equal (agent-switch-profile-id profile) "p-1234abcd"))
      (should (equal opened (agent-switch-profile-source profile)))
      (should (file-exists-p opened))
      (should (equal (agent-switch-profile-name
                      (agent-switch-find-profile "claude" "p-1234abcd"))
                     "File Profile")))))

(ert-deftest agent-switch-profile-edit-uses-standard-file-visit ()
  (agent-switch-test--with-root root
    (let* ((profile (agent-switch-save-profile
                     (agent-switch-test--profile
                      "claude" "editable"
                      (agent-switch-test--hash
                       "env" (agent-switch-test--hash
                              "ANTHROPIC_BASE_URL" "https://edit.test")))))
           opened)
      (cl-letf (((symbol-function 'agent-switch--profile-at-point-noerror)
                 (lambda () profile))
                ((symbol-function 'find-file)
                 (lambda (path) (setq opened path))))
        (agent-switch-profile-edit))
      (should (equal opened (agent-switch-profile-source profile))))))

(ert-deftest agent-switch-builtins-provide-profile-templates ()
  (agent-switch-test--with-root root
    (dolist (client-id '("claude" "codex" "gptel-default" "opencode-global"))
      (let* ((client (agent-switch-get-client client-id))
             (payload (agent-switch--new-profile-payload client)))
        (should (hash-table-p payload))))
    (let* ((claude (agent-switch--new-profile-payload
                    (agent-switch-get-client "claude")))
           (env (gethash "env" claude)))
      (should-not (gethash "ANTHROPIC_AUTH_TOKEN" env)))
    (let ((codex (agent-switch--new-profile-payload
                  (agent-switch-get-client "codex"))))
      (should (equal (gethash "wire_api" (gethash "provider" codex))
                     "responses")))))

(ert-deftest agent-switch-apply-records-payload-snapshot ()
  (agent-switch-test--with-root root
    (let* ((payload (agent-switch-test--hash
                     "env" (agent-switch-test--hash
                            "ANTHROPIC_BASE_URL" "https://snapshot.test")))
           (profile (agent-switch-test--profile
                     "claude" "snapshot" payload)))
      (agent-switch-state-set-last-selected "claude" "snapshot" profile)
      (let ((applied (agent-switch-state-applied-profile "claude")))
        (should (hash-table-p applied))
        (should (equal (gethash "fingerprint" applied)
                       (agent-switch-profile-payload-fingerprint profile)))
        (should (equal
                 (agent-switch-json-get-in
                  (gethash "payload" applied)
                  '("env" "ANTHROPIC_BASE_URL"))
                 "https://snapshot.test"))))))

(ert-deftest agent-switch-capture-current-removes-secret-markers ()
  (let* ((current
          (agent-switch-test--hash
           "env" (agent-switch-test--hash
                  "ANTHROPIC_BASE_URL" "https://capture.test"
                  "ANTHROPIC_AUTH_TOKEN"
                  (agent-switch--secret-marker "do-not-store"))))
         (captured (agent-switch--capture-current nil current nil))
         (env (gethash "env" captured)))
    (should (equal (gethash "ANTHROPIC_BASE_URL" env)
                   "https://capture.test"))
    (should-not (gethash "ANTHROPIC_AUTH_TOKEN" env))))

(provide 'agent-switch-test)

;;; agent-switch-test.el ends here
