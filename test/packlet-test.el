;;; packlet-test.el --- Tests for packlet -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for `packlet'.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'packlet)

(defvar packlet-test-command-ran)
(defvar packlet-test-hook-ran)
(defvar packlet-test-helper-ran)
(defvar packlet-test-custom-value)
(defvar packlet-test-compile-variable)
(defvar packlet-test-compile-function-ran)
(defvar packlet-test-compile-config-ran)
(defvar packlet-test-compile-second-config-ran)
(defvar packlet-test-reeval-result)
(defvar packlet-test-hook-count)

(declare-function packlet-test-hook "packlet-test-feature" ())
(declare-function packlet-test-startup-hook "packlet-test-multi-hook-feature" ())
(declare-function packlet-test-major-mode-hook "packlet-test-multi-hook-feature" ())
(declare-function packlet-test-al-custom-file "packlet-test-al-lib" ())

(defun packlet-test--write-file (file contents)
  "Write CONTENTS to FILE."
  (with-temp-file file
    (insert contents)
    (unless (string-suffix-p "\n" contents)
      (insert "\n"))))

(defun packlet-test--write-feature (directory feature body)
  "Write FEATURE with BODY into DIRECTORY."
  (packlet-test--write-file
   (expand-file-name (format "%s.el" feature) directory)
   (format ";;; %s.el --- Generated test feature -*- lexical-binding: t; -*-\n\n%s\n(provide '%s)\n"
           feature
           body
           feature)))

(defun packlet-test--write-library (directory file feature body)
  "Write FILE.el into DIRECTORY and make it provide FEATURE."
  (packlet-test--write-file
   (expand-file-name (format "%s.el" file) directory)
   (format ";;; %s.el --- Generated test library -*- lexical-binding: t; -*-\n\n%s\n(provide '%s)\n"
           file
           body
           feature)))

(defun packlet-test--cleanup-feature (feature)
  "Unload FEATURE and clear generated helper symbols."
  (when (featurep feature)
    (ignore-errors (unload-feature feature t)))
  (let ((configured-prefix (format "packlet--configured-%s-" feature))
        (configure-prefix (format "packlet--configure-%s-" feature)))
    (mapatoms
     (lambda (symbol)
       (let ((name (symbol-name symbol)))
         (when (or (string-prefix-p configured-prefix name)
                   (string-prefix-p configure-prefix name))
           (when (boundp symbol)
             (makunbound symbol))
           (when (fboundp symbol)
             (fmakunbound symbol))
           (unintern name nil)))))))

(defun packlet-test--cleanup-symbols (symbols)
  "Unbind and undefine SYMBOLS."
  (dolist (symbol symbols)
    (when (fboundp symbol)
      (fmakunbound symbol))
    (when (boundp symbol)
      (makunbound symbol))))

(defun packlet-test--invoke-scheduled-timer (entry)
  "Invoke a fake timer ENTRY captured by test stubs."
  (apply (plist-get entry :fn) (plist-get entry :args)))

(defun packlet-test--nest-progns (depth form)
  "Wrap FORM in DEPTH nested `progn' forms."
  (dotimes (_ depth form)
    (setq form `(progn ,form))))

(ert-deftest packlet-test-parse-body ()
  (should
   (equal
    (packlet--parse-body
     '(:init (setq a 1)
       :commands command-a
       :commands (command-b command-c)
       :config (message "configured")))
    '((:init (setq a 1))
      (:commands command-a (command-b command-c))
      (:config (message "configured"))))))

(ert-deftest packlet-test-run-source-entry-cleanups-continues-after-error ()
  (let ((scope '(:buffer cleanup-buffer))
        (calls nil)
        (warnings nil)
        (fail-first t))
    (cl-letf (((symbol-function 'display-warning)
               (lambda (_type message &optional _level)
                 (push message warnings))))
      (let ((failed
             (packlet--run-source-entry-cleanups
              (list
               (packlet--make-source-entry
                :id 'first
                :install #'ignore
                :cleanup (lambda ()
                           (setq calls (append calls '(:first)))))
               (packlet--make-source-entry
                :id 'second
                :install #'ignore
                :cleanup (lambda ()
                           (if fail-first
                               (progn
                                 (setq fail-first nil)
                                 (error "boom"))
                             (setq calls (append calls '(:recovered))))))
               (packlet--make-source-entry
                :id 'third
                :install #'ignore
                :cleanup (lambda ()
                           (setq calls (append calls '(:last))))))
              scope)))
        (should (equal calls '(:last :first)))
        (should (= (length warnings) 1))
        (should (= (length failed) 1))
        (packlet--run-source-entry-cleanups failed scope)
        (should (equal calls '(:last :first :recovered)))))))

(ert-deftest packlet-test-source-session-preserves-old-cleanup-failures-on-success ()
  (let ((scope '(:buffer keep-failed-buffer))
        (warnings nil))
    (cl-letf (((symbol-function 'display-warning)
               (lambda (_type message &optional _level)
                 (push message warnings))))
      (packlet--set-source-entries
       scope
       (list
        (packlet--make-source-entry
         :id 'stale
         :install #'ignore
         :cleanup (lambda ()
                    (error "boom")))))
      (packlet--with-source-session
       scope
       nil
       (lambda ()
         (packlet--register-source-entry
          scope
          'fresh
          #'ignore
          #'ignore)))
      (should (= (length warnings) 1))
      (should (equal (mapcar #'packlet--source-entry-id
                             (packlet--source-entries scope))
                     '(stale fresh))))))

(ert-deftest packlet-test-command-autoload-and-config ()
  (let* ((directory (make-temp-file "packlet-test-" t))
         (load-path (cons directory load-path))
         (auto-mode-alist auto-mode-alist)
         (emacs-startup-hook nil)
         (configured-value nil)
         (init-value nil)
         (key (kbd "C-c p"))
         (old-binding (lookup-key global-map key)))
    (unwind-protect
        (progn
          (packlet-test--cleanup-feature 'packlet-test-feature)
          (packlet-test--cleanup-symbols
           '(packlet-test-command
             packlet-test-hook
             packlet-test-command-ran
             packlet-test-hook-ran))
          (packlet-test--write-feature
           directory
           'packlet-test-feature
           "(defvar packlet-test-command-ran nil)\n\
(defvar packlet-test-hook-ran nil)\n\
(defun packlet-test-command ()\n\
  (interactive)\n\
  (setq packlet-test-command-ran t))\n\
(defun packlet-test-hook ()\n\
  (setq packlet-test-hook-ran t))")
          (packlet packlet-test-feature
            :init (setq init-value t)
            :commands packlet-test-command
            :mode ("\\\\.packlet-test\\\\'" . packlet-test-command)
            :hook (emacs-startup-hook . packlet-test-hook)
            :bind ("C-c p" . packlet-test-command)
            :config (setq configured-value t))
          (should init-value)
          (should (autoloadp (symbol-function 'packlet-test-command)))
          (should (eq (lookup-key global-map key) 'packlet-test-command))
          (should (equal (cdr (assoc "\\\\.packlet-test\\\\'" auto-mode-alist))
                         'packlet-test-command))
          (should-not configured-value)
          (require 'packlet-test-feature)
          (should configured-value)
          (run-hooks 'emacs-startup-hook)
          (should (bound-and-true-p packlet-test-hook-ran))
          (call-interactively 'packlet-test-command)
          (should (bound-and-true-p packlet-test-command-ran)))
      (packlet-test--cleanup-feature 'packlet-test-feature)
      (packlet-test--cleanup-symbols
       '(packlet-test-command
         packlet-test-hook
         packlet-test-command-ran
         packlet-test-hook-ran))
      (if old-binding
          (global-set-key key old-binding)
        (global-unset-key key))
      (delete-directory directory t))))

(ert-deftest packlet-test-multiple-hooks-register-and-run ()
  (let* ((directory (make-temp-file "packlet-test-" t))
         (load-path (cons directory load-path))
         (emacs-startup-hook nil)
         (after-change-major-mode-hook nil))
    (unwind-protect
        (progn
          (packlet-test--cleanup-feature 'packlet-test-multi-hook-feature)
          (packlet-test--cleanup-symbols
           '(packlet-test-startup-hook
             packlet-test-major-mode-hook
             packlet-test-startup-hook-ran
             packlet-test-major-mode-hook-ran))
          (packlet-test--write-feature
           directory
           'packlet-test-multi-hook-feature
           "(defvar packlet-test-startup-hook-ran nil)\n\
(defvar packlet-test-major-mode-hook-ran nil)\n\
(defun packlet-test-startup-hook ()\n\
  (setq packlet-test-startup-hook-ran t))\n\
(defun packlet-test-major-mode-hook ()\n\
  (setq packlet-test-major-mode-hook-ran t))")
          (packlet packlet-test-multi-hook-feature
            :hook ((emacs-startup-hook . packlet-test-startup-hook)
                   (after-change-major-mode-hook . packlet-test-major-mode-hook)))
          (run-hooks 'emacs-startup-hook)
          (run-hooks 'after-change-major-mode-hook)
          (should (bound-and-true-p packlet-test-startup-hook-ran))
          (should (bound-and-true-p packlet-test-major-mode-hook-ran)))
      (packlet-test--cleanup-feature 'packlet-test-multi-hook-feature)
      (packlet-test--cleanup-symbols
       '(packlet-test-startup-hook
         packlet-test-major-mode-hook
         packlet-test-startup-hook-ran
         packlet-test-major-mode-hook-ran))
      (delete-directory directory t))))

(ert-deftest packlet-test-demand-load-after-feature ()
  (let* ((directory (make-temp-file "packlet-test-" t))
         (load-path (cons directory load-path)))
    (unwind-protect
        (progn
          (packlet-test--cleanup-feature 'packlet-test-demand)
          (packlet-test--cleanup-feature 'packlet-test-after)
          (packlet-test--write-feature
           directory
           'packlet-test-after
           "(defvar packlet-test-after-loaded t)")
          (packlet-test--write-feature
           directory
           'packlet-test-demand
           "(defvar packlet-test-demand-loaded t)")
          (packlet packlet-test-demand
            :after packlet-test-after
            :demand t)
          (should-not (featurep 'packlet-test-demand))
          (require 'packlet-test-after)
          (should (featurep 'packlet-test-demand)))
      (packlet-test--cleanup-feature 'packlet-test-demand)
      (packlet-test--cleanup-feature 'packlet-test-after)
      (delete-directory directory t))))

(ert-deftest packlet-test-multiple-packlet-configs-run-for-same-feature ()
  (let* ((directory (make-temp-file "packlet-test-" t))
         (load-path (cons directory load-path))
         (config-a-ran nil)
         (config-b-ran nil))
    (unwind-protect
        (progn
          (packlet-test--cleanup-feature 'packlet-test-shared-config)
          (packlet-test--write-feature
           directory
           'packlet-test-shared-config
           "(defvar packlet-test-shared-config-loaded t)")
          (packlet packlet-test-shared-config
            :config
            (setq config-a-ran t))
          (packlet packlet-test-shared-config
            :config
            (setq config-b-ran t))
          (should-not config-a-ran)
          (should-not config-b-ran)
          (require 'packlet-test-shared-config)
          (should config-a-ran)
          (should config-b-ran))
      (packlet-test--cleanup-feature 'packlet-test-shared-config)
      (delete-directory directory t))))

(ert-deftest packlet-test-config-handlers-run-in-registration-order ()
  (let* ((directory (make-temp-file "packlet-test-" t))
         (load-path (cons directory load-path))
         (feature 'packlet-test-config-order)
         (result nil))
    (unwind-protect
        (progn
          (packlet-test--cleanup-feature feature)
          (packlet-test--write-feature
           directory
           feature
           "(defvar packlet-test-config-order-loaded t)")
          (packlet packlet-test-config-order
            :config
            (setq result (append result '(a))))
          (packlet packlet-test-config-order
            :config
            (setq result (append result '(b))))
          (require feature)
          (should (equal result '(a b))))
      (packlet-test--cleanup-feature feature)
      (delete-directory directory t))))

(ert-deftest packlet-test-config-reeval-does-not-duplicate ()
  (let* ((directory (make-temp-file "packlet-test-" t))
         (load-path (cons directory load-path))
         (feature 'packlet-test-reeval-config))
    (unwind-protect
        (progn
          (setq packlet-test-reeval-result nil)
          (packlet-test--cleanup-feature feature)
          (packlet-test--write-feature
           directory
           feature
           "(defvar packlet-test-reeval-config-loaded t)")
          (eval
           '(packlet packlet-test-reeval-config
              :config
              (push 'ran packlet-test-reeval-result)))
          (eval
           '(packlet packlet-test-reeval-config
              :config
              (push 'ran packlet-test-reeval-result)))
          (should-not packlet-test-reeval-result)
          (require feature)
          (should (equal packlet-test-reeval-result '(ran))))
      (setq packlet-test-reeval-result nil)
      (packlet-test--cleanup-feature feature)
      (delete-directory directory t))))

(ert-deftest packlet-test-config-reeval-updates-same-site ()
  (let* ((directory (make-temp-file "packlet-test-" t))
         (load-path (cons directory load-path))
         (feature 'packlet-test-reeval-buffer)
         (source-file (expand-file-name "packlet-test-reeval-buffer.el" directory)))
    (unwind-protect
        (progn
          (setq packlet-test-reeval-result nil)
          (packlet-test--cleanup-feature feature)
          (packlet-test--write-feature
           directory
           feature
           "(defvar packlet-test-reeval-buffer-loaded t)")
          (with-temp-buffer
            (emacs-lisp-mode)
            (setq buffer-file-name source-file)
            (insert "(packlet packlet-test-reeval-buffer\n\
  :config\n\
  (push 'old packlet-test-reeval-result))\n")
            (goto-char (point-min))
            (eval-buffer)
            (erase-buffer)
            (insert "(packlet packlet-test-reeval-buffer\n\
  :config\n\
  (push 'new packlet-test-reeval-result))\n")
            (goto-char (point-min))
            (eval-buffer))
          (require feature)
          (should (equal packlet-test-reeval-result '(new))))
      (setq packlet-test-reeval-result nil)
      (packlet-test--cleanup-feature feature)
      (delete-directory directory t))))

(ert-deftest packlet-test-multiple-packlets-in-same-top-level-form-do-not-collide ()
  (let* ((directory (make-temp-file "packlet-test-" t))
         (load-path (cons directory load-path))
         (feature 'packlet-test-shared-top-level))
    (unwind-protect
        (progn
          (setq packlet-test-reeval-result nil)
          (packlet-test--cleanup-feature feature)
          (packlet-test--write-feature
           directory
           feature
           "(defvar packlet-test-shared-top-level-loaded t)")
          (with-temp-buffer
            (emacs-lisp-mode)
            (setq buffer-file-name
                  (expand-file-name "packlet-test-shared-top-level-config.el"
                                    directory))
            (insert "(progn\n\
  (packlet packlet-test-shared-top-level\n\
    :config\n\
    (push 'a packlet-test-reeval-result))\n\
  (packlet packlet-test-shared-top-level\n\
    :config\n\
    (push 'b packlet-test-reeval-result)))\n")
            (goto-char (point-min))
            (eval-buffer))
          (require feature)
          (should (equal (sort (copy-sequence packlet-test-reeval-result)
                               (lambda (left right)
                                 (string-lessp (symbol-name left)
                                               (symbol-name right))))
                         '(a b))))
      (setq packlet-test-reeval-result nil)
      (packlet-test--cleanup-feature feature)
      (delete-directory directory t))))

(ert-deftest packlet-test-config-reeval-after-form-move-does-not-leak-old-site ()
  (let* ((directory (make-temp-file "packlet-test-" t))
         (load-path (cons directory load-path))
         (feature 'packlet-test-reeval-moved)
         (source-file (expand-file-name "packlet-test-reeval-moved.el" directory)))
    (unwind-protect
        (progn
          (setq packlet-test-reeval-result nil)
          (packlet-test--cleanup-feature feature)
          (packlet-test--write-feature
           directory
           feature
           "(defvar packlet-test-reeval-moved-loaded t)")
          (with-temp-buffer
            (emacs-lisp-mode)
            (setq buffer-file-name source-file)
            (insert "(packlet packlet-test-reeval-moved\n\
  :config\n\
  (push 'old packlet-test-reeval-result))\n")
            (goto-char (point-min))
            (eval-buffer)
            (erase-buffer)
            (insert "\n(packlet packlet-test-reeval-moved\n\
  :config\n\
  (push 'new packlet-test-reeval-result))\n")
            (goto-char (point-min))
            (eval-buffer))
          (require feature)
          (should (equal packlet-test-reeval-result '(new))))
      (setq packlet-test-reeval-result nil)
      (packlet-test--cleanup-feature feature)
      (delete-directory directory t))))

(ert-deftest packlet-test-config-reeval-after-form-removal-clears-old-site ()
  (let* ((directory (make-temp-file "packlet-test-" t))
         (load-path (cons directory load-path))
         (feature 'packlet-test-reeval-removed)
         (source-file (expand-file-name "packlet-test-reeval-removed.el" directory)))
    (unwind-protect
        (progn
          (setq packlet-test-reeval-result nil)
          (packlet-test--cleanup-feature feature)
          (packlet-test--write-feature
           directory
           feature
           "(defvar packlet-test-reeval-removed-loaded t)")
          (with-temp-buffer
            (emacs-lisp-mode)
            (setq buffer-file-name source-file)
            (insert "(packlet packlet-test-reeval-removed\n\
  :config\n\
  (push 'old packlet-test-reeval-result))\n")
            (goto-char (point-min))
            (eval-buffer)
            (erase-buffer)
            (insert ";; removed\n")
            (goto-char (point-min))
            (eval-buffer))
          (require feature)
          (should-not packlet-test-reeval-result))
      (setq packlet-test-reeval-result nil)
      (packlet-test--cleanup-feature feature)
      (delete-directory directory t))))

(ert-deftest packlet-test-file-reeval-error-rolls-back-old-hook ()
  (let ((source-file (make-temp-file "packlet-test-rollback-" nil ".el")))
    (defvar packlet-test-rollback-hook nil)
    (unwind-protect
        (progn
          (setq packlet-test-hook-count 0
                packlet-test-rollback-hook nil)
          (with-temp-buffer
            (emacs-lisp-mode)
            (setq buffer-file-name source-file)
            (insert "(packlet packlet-test-rollback-feature\n\
  :hook ((packlet-test-rollback-hook\n\
          . (lambda ()\n\
              (setq packlet-test-hook-count (1+ packlet-test-hook-count))))))\n")
            (goto-char (point-min))
            (eval-buffer)
            (erase-buffer)
            (insert "(packlet packlet-test-rollback-feature\n\
  :hook ((packlet-test-rollback-hook\n\
          . (lambda ()\n\
              (setq packlet-test-hook-count (+ packlet-test-hook-count 10))))))\n\
(this-symbol-does-not-exist)\n")
            (goto-char (point-min))
            (should-error (eval-buffer)))
          (run-hooks 'packlet-test-rollback-hook)
          (should (= packlet-test-hook-count 1)))
      (setq packlet-test-hook-count nil)
      (packlet-test--cleanup-symbols '(packlet-test-rollback-hook))
      (when (file-exists-p source-file)
        (delete-file source-file)))))

(ert-deftest packlet-test-idle-keyword-registers-loader ()
  (let (calls)
    (cl-letf (((symbol-function 'packlet--register-idle-load)
               (lambda (id feature afters file delay source-file)
                 (push (list id feature afters file delay source-file) calls))))
      (packlet packlet-test-idle-default
        :idle)
      (packlet packlet-test-idle-delayed
        :after packlet-test-after
        :idle 2.5))
    (should
     (equal (mapcar (lambda (call) (cdr (butlast call))) (nreverse calls))
            '((packlet-test-idle-default nil "packlet-test-idle-default" 1.0)
              (packlet-test-idle-delayed
               (packlet-test-after)
               "packlet-test-idle-delayed"
               2.5))))))

(ert-deftest packlet-test-nonfile-eval-replaces-old-hook ()
  (defvar packlet-test-nonfile-hook nil)
  (unwind-protect
      (with-temp-buffer
        (emacs-lisp-mode)
        (setq packlet-test-hook-count 0
              packlet-test-nonfile-hook nil)
        (eval
         '(packlet packlet-test-nonfile-feature
            :hook ((packlet-test-nonfile-hook
                    . (lambda ()
                        (setq packlet-test-hook-count
                              (1+ packlet-test-hook-count)))))))
        (eval
         '(packlet packlet-test-nonfile-feature
            :hook ((packlet-test-nonfile-hook
                    . (lambda ()
                        (setq packlet-test-hook-count
                              (+ packlet-test-hook-count 10)))))))
        (run-hooks 'packlet-test-nonfile-hook)
        (should (= packlet-test-hook-count 10)))
    (setq packlet-test-hook-count nil)
    (packlet-test--cleanup-symbols '(packlet-test-nonfile-hook))))

(ert-deftest packlet-test-kill-buffer-clears-nonfile-source-state ()
  (defvar packlet-test-buffer-scope-hook nil)
  (let ((buffer (generate-new-buffer " *packlet-test-buffer-scope*"))
        scope)
    (unwind-protect
        (progn
          (setq packlet-test-hook-count 0
                packlet-test-buffer-scope-hook nil)
          (with-current-buffer buffer
            (emacs-lisp-mode)
            (setq scope (list :buffer buffer))
            (eval
             '(packlet packlet-test-buffer-scope-feature
                :hook ((packlet-test-buffer-scope-hook
                        . (lambda ()
                            (setq packlet-test-hook-count
                                  (1+ packlet-test-hook-count))))))))
          (should (packlet--source-entries scope))
          (kill-buffer buffer)
          (setq buffer nil)
          (run-hooks 'packlet-test-buffer-scope-hook)
          (should (= packlet-test-hook-count 0))
          (should-not (packlet--source-entries scope)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (setq packlet-test-hook-count nil)
      (packlet-test--cleanup-symbols '(packlet-test-buffer-scope-hook)))))

(ert-deftest packlet-test-deep-nonfile-eval-detects-packlet ()
  (defvar packlet-test-deep-eval-hook nil)
  (unwind-protect
      (with-temp-buffer
        (emacs-lisp-mode)
        (setq packlet-test-hook-count 0
              packlet-test-deep-eval-hook nil)
        (eval
         (packlet-test--nest-progns
          40
          '(packlet packlet-test-deep-eval-feature
             :hook ((packlet-test-deep-eval-hook
                     . (lambda ()
                         (setq packlet-test-hook-count
                               (1+ packlet-test-hook-count))))))))
        (eval
         (packlet-test--nest-progns
          40
          '(packlet packlet-test-deep-eval-feature
             :hook ((packlet-test-deep-eval-hook
                     . (lambda ()
                         (setq packlet-test-hook-count
                               (+ packlet-test-hook-count 10))))))))
        (run-hooks 'packlet-test-deep-eval-hook)
        (should (= packlet-test-hook-count 10)))
    (setq packlet-test-hook-count nil)
    (packlet-test--cleanup-symbols '(packlet-test-deep-eval-hook))))

(ert-deftest packlet-test-idle-load-waits-for-startup ()
  (let ((after-init-time nil)
        (emacs-startup-hook nil)
        (scheduled nil)
        (load-count 0)
        (timer-id 0))
    (cl-letf (((symbol-function 'run-with-idle-timer)
               (lambda (secs repeat fn &rest args)
                 (let ((timer (list :timer (cl-incf timer-id))))
                   (push (list :timer timer :secs secs :repeat repeat
                               :fn fn :args args)
                         scheduled)
                   timer)))
              ((symbol-function 'timerp)
               (lambda (object)
                 (and (consp object)
                      (eq (car object) :timer))))
              ((symbol-function 'packlet--idle-load-ready-p)
               (lambda ()
                 t))
              ((symbol-function 'packlet--load-feature)
               (lambda (_feature _file)
                 (setq load-count (1+ load-count))
                 t)))
      (packlet--register-idle-load
       'packlet-test-idle-loader
       'packlet-test-idle
       nil
       "packlet-test-idle"
       2.5)
      (should-not scheduled)
      (run-hooks 'emacs-startup-hook)
      (should (= (length scheduled) 1))
      (should (= (plist-get (car scheduled) :secs) 2.5))
      (packlet-test--invoke-scheduled-timer (car scheduled))
      (should (= load-count 1)))))

(ert-deftest packlet-test-idle-load-retries-while-busy ()
  (let ((after-init-time (current-time))
        (scheduled nil)
        (load-count 0)
        (timer-id 0)
        (ready-values '(nil t)))
    (cl-letf (((symbol-function 'run-with-idle-timer)
               (lambda (secs repeat fn &rest args)
                 (let ((timer (list :timer (cl-incf timer-id))))
                   (push (list :timer timer :secs secs :repeat repeat
                               :fn fn :args args)
                         scheduled)
                   timer)))
              ((symbol-function 'timerp)
               (lambda (object)
                 (and (consp object)
                      (eq (car object) :timer))))
              ((symbol-function 'packlet--idle-load-ready-p)
               (lambda ()
                 (prog1 (car ready-values)
                   (setq ready-values
                         (or (cdr ready-values) ready-values)))))
              ((symbol-function 'packlet--load-feature)
               (lambda (_feature _file)
                 (setq load-count (1+ load-count))
                 t)))
          (packlet--register-idle-load
           'packlet-test-idle-loader
           'packlet-test-idle
           nil
           "packlet-test-idle"
           1.0)
      (should (= (length scheduled) 1))
      (packlet-test--invoke-scheduled-timer (car scheduled))
      (should (= load-count 0))
      (should (= (length scheduled) 2))
      (packlet-test--invoke-scheduled-timer (car scheduled))
      (should (= load-count 1)))))

(ert-deftest packlet-test-idle-load-waits-for-after-feature ()
  (let* ((directory (make-temp-file "packlet-test-" t))
         (load-path (cons directory load-path))
         (after-init-time (current-time))
         (scheduled nil)
         (timer-id 0))
    (unwind-protect
        (cl-letf (((symbol-function 'run-with-idle-timer)
                   (lambda (secs repeat fn &rest args)
                     (let ((timer (list :timer (cl-incf timer-id))))
                       (push (list :timer timer :secs secs :repeat repeat
                                   :fn fn :args args)
                             scheduled)
                       timer)))
                  ((symbol-function 'timerp)
                   (lambda (object)
                     (and (consp object)
                          (eq (car object) :timer))))
                  ((symbol-function 'packlet--idle-load-ready-p)
                   (lambda ()
                     t))
                  ((symbol-function 'packlet--load-feature)
                   (lambda (_feature _file)
                     t)))
          (packlet-test--cleanup-feature 'packlet-test-idle-after)
          (packlet-test--write-feature
           directory
           'packlet-test-idle-after
           "(defvar packlet-test-idle-after-loaded t)")
          (packlet--register-idle-load
           'packlet-test-idle-loader
           'packlet-test-idle
           '(packlet-test-idle-after)
           "packlet-test-idle"
           1.0)
          (should-not scheduled)
          (require 'packlet-test-idle-after)
          (should (= (length scheduled) 1)))
      (packlet-test--cleanup-feature 'packlet-test-idle-after)
      (delete-directory directory t))))

(ert-deftest packlet-test-file-autoload-custom-and-keymap-bindings ()
  (let* ((directory (make-temp-file "packlet-test-" t))
         (load-path (cons directory load-path))
         (feature 'packlet-test-renamed-feature)
         (file 'packlet-test-renamed-library)
         (symbols '(packlet-test-helper
                    packlet-test-mode-command
                    packlet-test-mode-map
                    packlet-test-helper-ran
                    packlet-test-custom-value)))
    (unwind-protect
        (progn
          (packlet-test--cleanup-feature feature)
          (packlet-test--cleanup-symbols symbols)
          (packlet-test--write-library
           directory
           file
           feature
           "(defvar packlet-test-helper-ran nil)\n\
(defvar packlet-test-mode-map (make-sparse-keymap))\n\
(defun packlet-test-helper ()\n\
  (setq packlet-test-helper-ran t))\n\
(defun packlet-test-mode-command ()\n\
  (interactive))")
          (eval
           `(packlet ,feature
              :file ,file
              :autoload packlet-test-helper
              :custom (packlet-test-custom-value 42)
              :bind (:map packlet-test-mode-map
                     ("C-c C-t" . packlet-test-mode-command))))
          (should (= packlet-test-custom-value 42))
          (should (autoloadp (symbol-function 'packlet-test-helper)))
          (should (equal (nth 1 (symbol-function 'packlet-test-helper))
                         "packlet-test-renamed-library"))
          (should-not (featurep feature))
          (funcall (intern "packlet-test-helper"))
          (should (featurep feature))
          (should (bound-and-true-p packlet-test-helper-ran))
          (should (eq (lookup-key (symbol-value 'packlet-test-mode-map)
                                  (kbd "C-c C-t"))
                      'packlet-test-mode-command)))
      (packlet-test--cleanup-feature feature)
      (packlet-test--cleanup-symbols symbols)
      (delete-directory directory t))))

(ert-deftest packlet-test-package-autoloads-are-preferred ()
  (let* ((directory (make-temp-file "packlet-test-" t))
         (load-path (cons directory load-path))
         (feature 'packlet-test-package)
         (symbols '(packlet-test-package-command
                    packlet-test-package-command-ran
                    packlet-test-package-config-ran))
         (key (kbd "C-c C-p"))
         (old-binding (lookup-key global-map key)))
    (unwind-protect
        (progn
          (packlet-test--cleanup-feature feature)
          (packlet-test--cleanup-symbols symbols)
          (packlet-test--write-library
           directory
           'packlet-test-package-command
           feature
           "(defvar packlet-test-package-command-ran nil)\n\
(defvar packlet-test-package-config-ran nil)\n\
(defun packlet-test-package-command ()\n\
  (interactive)\n\
  (setq packlet-test-package-command-ran t))")
          (packlet-test--write-file
           (expand-file-name "packlet-test-package-autoloads.el" directory)
           ";;; packlet-test-package-autoloads.el --- Generated autoloads -*- lexical-binding: t; -*-\n\n\
(autoload 'packlet-test-package-command \"packlet-test-package-command\" nil t)\n\
(provide 'packlet-test-package-autoloads)\n")
          (eval
           `(packlet ,feature
              :commands packlet-test-package-command
              :bind ("C-c C-p" . packlet-test-package-command)
              :config
              (setq packlet-test-package-config-ran t)))
          (should (autoloadp (symbol-function 'packlet-test-package-command)))
          (should (equal (nth 1 (symbol-function 'packlet-test-package-command))
                         "packlet-test-package-command"))
          (should (eq (lookup-key global-map key)
                      'packlet-test-package-command))
          (call-interactively 'packlet-test-package-command)
          (should (featurep feature))
          (should (bound-and-true-p packlet-test-package-command-ran))
          (should (bound-and-true-p packlet-test-package-config-ran)))
      (packlet-test--cleanup-feature feature)
      (packlet-test--cleanup-symbols symbols)
      (if old-binding
          (global-set-key key old-binding)
        (global-unset-key key))
      (delete-directory directory t))))

(ert-deftest packlet-test-maybe-autoload-upgrades-fallback-definition ()
  (let* ((fallback-directory (make-temp-file "packlet-test-fallback-" t))
         (autoload-directory (make-temp-file "packlet-test-autoloads-" t))
         (load-path (cons fallback-directory load-path))
         (autoloads-feature 'packlet-test-upgrade-feature-autoloads)
         (symbols '(packlet-test-upgrade-command
                    packlet-test-upgrade-command-ran)))
    (unwind-protect
        (progn
          (packlet-test--cleanup-feature autoloads-feature)
          (packlet-test--cleanup-feature 'packlet-test-upgrade-sub)
          (packlet-test--cleanup-symbols symbols)
          (remhash "packlet-test-upgrade-feature-autoloads"
                   packlet--loaded-autoloads)
          (packlet-test--write-library
           fallback-directory
           'packlet-test-upgrade-feature
           'packlet-test-upgrade-feature
           "(defvar packlet-test-upgrade-feature-loaded t)")
          (packlet-test--write-library
           autoload-directory
           'packlet-test-upgrade-sub
           'packlet-test-upgrade-sub
           "(defvar packlet-test-upgrade-command-ran nil)\n\
(defun packlet-test-upgrade-command ()\n\
  (interactive)\n\
  (setq packlet-test-upgrade-command-ran t))")
          (packlet-test--write-file
           (expand-file-name "packlet-test-upgrade-feature-autoloads.el"
                             autoload-directory)
           ";;; packlet-test-upgrade-feature-autoloads.el --- Generated autoloads -*- lexical-binding: t; -*-\n\n\
(autoload 'packlet-test-upgrade-command \"packlet-test-upgrade-sub\" nil t)\n\
(provide 'packlet-test-upgrade-feature-autoloads)\n")
          (packlet--maybe-autoload
           'packlet-test-upgrade-command
           "packlet-test-upgrade-feature"
           t)
          (should (autoloadp (symbol-function 'packlet-test-upgrade-command)))
          (should (equal (nth 1 (symbol-function 'packlet-test-upgrade-command))
                         "packlet-test-upgrade-feature"))
          (setq load-path (cons autoload-directory load-path))
          (packlet--maybe-autoload
           'packlet-test-upgrade-command
           "packlet-test-upgrade-feature"
           t)
          (should (equal (nth 1 (symbol-function 'packlet-test-upgrade-command))
                         "packlet-test-upgrade-sub"))
          (call-interactively 'packlet-test-upgrade-command)
          (should (bound-and-true-p packlet-test-upgrade-command-ran)))
      (packlet-test--cleanup-feature autoloads-feature)
      (packlet-test--cleanup-feature 'packlet-test-upgrade-sub)
      (packlet-test--cleanup-feature 'packlet-test-upgrade-feature)
      (packlet-test--cleanup-symbols symbols)
      (remhash "packlet-test-upgrade-feature-autoloads"
               packlet--loaded-autoloads)
      (delete-directory fallback-directory t)
      (delete-directory autoload-directory t))))

(ert-deftest packlet-test-file-demand-load ()
  (let* ((directory (make-temp-file "packlet-test-" t))
         (load-path (cons directory load-path))
         (feature 'packlet-test-demand-feature)
         (file 'packlet-test-demand-library))
    (unwind-protect
        (progn
          (packlet-test--cleanup-feature feature)
          (packlet-test--write-library
           directory
           file
           feature
           "(defvar packlet-test-demand-file-loaded t)")
          (should-not (featurep feature))
          (eval
           `(packlet ,feature
              :file ,file
              :demand t))
          (should (featurep feature)))
      (packlet-test--cleanup-feature feature)
      (delete-directory directory t))))

(ert-deftest packlet-test-functions-defines-and-custom-byte-compile ()
  (let* ((directory (make-temp-file "packlet-test-" t))
         (library-feature 'packlet-test-compile-feature)
         (library-file 'packlet-test-compile-library)
         (config-file (expand-file-name "packlet-test-config.el" directory))
         (compiled-file (concat config-file "c"))
         (packlet-directory (file-name-directory (locate-library "packlet")))
         (symbols '(packlet-test-compile-variable
                    packlet-test-compile-function
                    packlet-test-compile-function-ran
                    packlet-test-compile-config-ran
                    packlet-test-compile-second-config-ran)))
    (unwind-protect
        (progn
          (packlet-test--cleanup-feature library-feature)
          (packlet-test--cleanup-symbols symbols)
          (packlet-test--write-library
           directory
           library-file
           library-feature
           "(defvar packlet-test-compile-variable nil)\n\
(defvar packlet-test-compile-function-ran nil)\n\
(defun packlet-test-compile-function ()\n\
  (setq packlet-test-compile-function-ran packlet-test-compile-variable))")
          (packlet-test--write-file
           config-file
           ";;; packlet-test-config.el --- Generated test config -*- lexical-binding: t; -*-\n\n\
(require 'packlet)\n\n\
(defvar packlet-test-compile-config-ran nil)\n\
(defvar packlet-test-compile-second-config-ran nil)\n\n\
(packlet packlet-test-compile-feature\n\
  :file packlet-test-compile-library\n\
  :defines packlet-test-compile-variable\n\
  :functions packlet-test-compile-function\n\
  :custom (packlet-test-compile-variable 42)\n\
  :config\n\
  (setq packlet-test-compile-config-ran t)\n\
  (when packlet-test-compile-variable\n\
    (packlet-test-compile-function)))\n\n\
(packlet packlet-test-compile-feature\n\
  :config\n\
  (setq packlet-test-compile-second-config-ran t))")
          (let ((load-path (append (list directory packlet-directory) load-path))
                (byte-compile-error-on-warn t))
            (should (byte-compile-file config-file))
            (should (file-exists-p compiled-file))
            (load compiled-file nil t)
            (load (symbol-name library-file) nil t)
            (should (bound-and-true-p packlet-test-compile-config-ran))
            (should (bound-and-true-p packlet-test-compile-second-config-ran))
            (should (= packlet-test-compile-variable 42))
            (should (= packlet-test-compile-function-ran 42))))
      (when (file-exists-p compiled-file)
        (delete-file compiled-file))
      (packlet-test--cleanup-feature library-feature)
      (packlet-test--cleanup-symbols symbols)
      (delete-directory directory t))))

(ert-deftest packlet-test-autoload-with-file-and-interactive ()
  (let* ((directory (make-temp-file "packlet-test-" t))
         (load-path (cons directory load-path))
         (feature 'packlet-test-autoload-extended)
         (symbols '(packlet-test-al-simple
                    packlet-test-al-custom-file
                    packlet-test-al-interactive
                    packlet-test-al-simple-ran
                    packlet-test-al-custom-ran
                    packlet-test-al-interactive-ran)))
    (unwind-protect
        (progn
          (packlet-test--cleanup-feature feature)
          (packlet-test--cleanup-symbols symbols)
          ;; Main feature file
          (packlet-test--write-feature
           directory
           feature
           "(defvar packlet-test-al-simple-ran nil)\n\
(defun packlet-test-al-simple ()\n\
  (setq packlet-test-al-simple-ran t))")
          ;; Separate library for custom-file autoload
          (packlet-test--write-library
           directory
           'packlet-test-al-lib
           'packlet-test-al-lib
           "(defvar packlet-test-al-custom-ran nil)\n\
(defun packlet-test-al-custom-file ()\n\
  (setq packlet-test-al-custom-ran t))")
          ;; Separate library for interactive autoload
          (packlet-test--write-library
           directory
           'packlet-test-al-cmd
           'packlet-test-al-cmd
           "(defvar packlet-test-al-interactive-ran nil)\n\
(defun packlet-test-al-interactive ()\n\
  (interactive)\n\
  (setq packlet-test-al-interactive-ran t))")
          (eval
           `(packlet ,feature
              :autoload
              packlet-test-al-simple
              (packlet-test-al-custom-file "packlet-test-al-lib")
              (packlet-test-al-interactive "packlet-test-al-cmd" t)))
          ;; Bare symbol: autoloaded from default file, non-interactive
          (should (autoloadp (symbol-function 'packlet-test-al-simple)))
          (should (equal (nth 1 (symbol-function 'packlet-test-al-simple))
                         "packlet-test-autoload-extended"))
          (should-not (nth 3 (symbol-function 'packlet-test-al-simple)))
          ;; Tuple (fn "file"): autoloaded from custom file, non-interactive
          (should (autoloadp (symbol-function 'packlet-test-al-custom-file)))
          (should (equal (nth 1 (symbol-function 'packlet-test-al-custom-file))
                         "packlet-test-al-lib"))
          (should-not (nth 3 (symbol-function 'packlet-test-al-custom-file)))
          ;; Tuple (fn "file" t): autoloaded from custom file, interactive
          (should (autoloadp (symbol-function 'packlet-test-al-interactive)))
          (should (equal (nth 1 (symbol-function 'packlet-test-al-interactive))
                         "packlet-test-al-cmd"))
          (should (nth 3 (symbol-function 'packlet-test-al-interactive)))
          ;; Verify the autoloads actually work
          (funcall 'packlet-test-al-custom-file)
          (should (bound-and-true-p packlet-test-al-custom-ran))
          (call-interactively 'packlet-test-al-interactive)
          (should (bound-and-true-p packlet-test-al-interactive-ran)))
      (packlet-test--cleanup-feature feature)
      (packlet-test--cleanup-feature 'packlet-test-al-lib)
      (packlet-test--cleanup-feature 'packlet-test-al-cmd)
      (packlet-test--cleanup-symbols symbols)
      (delete-directory directory t))))

(ert-deftest packlet-test-setq-keyword ()
  (let ((packlet-test-setq-a nil)
        (packlet-test-setq-b nil))
    (packlet packlet-test-setq-feature
      :setq ((packlet-test-setq-a 42)
             (packlet-test-setq-b "hello")))
    (should (= packlet-test-setq-a 42))
    (should (equal packlet-test-setq-b "hello"))))

(ert-deftest packlet-test-load-keyword ()
  (let* ((directory (make-temp-file "packlet-test-" t))
         (load-path (cons directory load-path)))
    (unwind-protect
        (progn
          (packlet-test--write-file
           (expand-file-name "packlet-test-loadable.el" directory)
           "(defvar packlet-test-loaded-marker t)")
          (should-not (boundp 'packlet-test-loaded-marker))
          (packlet packlet-test-load-feature
            :load "packlet-test-loadable")
          (should (bound-and-true-p packlet-test-loaded-marker)))
      (when (boundp 'packlet-test-loaded-marker)
        (makunbound 'packlet-test-loaded-marker))
      (delete-directory directory t))))

(ert-deftest packlet-test-load-keyword-warns-when-library-missing ()
  (let (warnings)
    (cl-letf (((symbol-function 'display-warning)
               (lambda (_type message &optional _level)
                 (push message warnings))))
      (packlet packlet-test-missing-load
        :load "packlet-test-missing-library"))
    (should (= (length warnings) 1))
    (should (string-match-p
             "packlet-test-missing-library"
             (car warnings)))))

(ert-deftest packlet-test-demand-keyword-warns-when-library-missing ()
  (let (warnings)
    (cl-letf (((symbol-function 'display-warning)
               (lambda (_type message &optional _level)
                 (push message warnings))))
      (packlet packlet-test-missing-demand
        :demand t))
    (should (= (length warnings) 1))
    (should (string-match-p
             "packlet-test-missing-demand"
             (car warnings)))))

(ert-deftest packlet-test-after-load-keyword ()
  (let* ((directory (make-temp-file "packlet-test-" t))
         (load-path (cons directory load-path))
         (after-load-result nil))
    (unwind-protect
        (progn
          (packlet-test--cleanup-feature 'packlet-test-after-load-dep)
          (packlet-test--write-feature
           directory
           'packlet-test-after-load-dep
           "(defvar packlet-test-after-load-dep-loaded t)")
          (packlet packlet-test-after-load-feature
            :after-load
            (packlet-test-after-load-dep
             (setq after-load-result t)))
          (should-not after-load-result)
          (require 'packlet-test-after-load-dep)
          (should after-load-result))
      (packlet-test--cleanup-feature 'packlet-test-after-load-dep)
      (delete-directory directory t))))

(ert-deftest packlet-test-after-load-dispatcher-clears-when-last-handler-removed ()
  (let ((feature 'packlet-test-dispatcher-clear))
    (unwind-protect
        (progn
          (clrhash packlet--after-load-handlers)
          (clrhash packlet--after-load-dispatchers)
          (packlet--register-after-load-handler
           feature
           'first
           #'ignore)
          (should (gethash feature packlet--after-load-dispatchers))
          (should (packlet--after-load-handler-entries feature))
          (packlet--unregister-after-load-handler feature 'first)
          (should-not (packlet--after-load-handler-entries feature))
          (should-not (gethash feature packlet--after-load-dispatchers)))
      (clrhash packlet--after-load-handlers)
      (clrhash packlet--after-load-dispatchers))))

(ert-deftest packlet-test-user-defined-keyword ()
  (defvar packlet-test-user-kw-result nil)
  (let ((packlet--user-keywords nil))
    (unwind-protect
        (progn
          (setq packlet-test-user-kw-result nil)
          (packlet-define-keyword :my-test-kw
            :normalize (lambda (forms)
                         (mapcar #'symbol-name
                                 (packlet--normalize-symbols forms :my-test-kw)))
            :expand (lambda (ctx forms)
                      (let ((feature (plist-get ctx :feature)))
                        (mapcar (lambda (name)
                                  `(push (cons ',feature ,name)
                                         packlet-test-user-kw-result))
                                forms))))
          (should (assq :my-test-kw packlet--user-keywords))
          (eval '(packlet packlet-test-user-kw-feature
                   :my-test-kw (alpha beta)))
          (should (equal (nreverse packlet-test-user-kw-result)
                         '((packlet-test-user-kw-feature . "alpha")
                           (packlet-test-user-kw-feature . "beta")))))
      (setq packlet-test-user-kw-result nil))))

(ert-deftest packlet-test-user-keywords-follow-section-order ()
  (defvar packlet-test-user-kw-order nil)
  (let ((packlet--user-keywords nil))
    (unwind-protect
        (progn
          (setq packlet-test-user-kw-order nil)
          (packlet-define-keyword :packlet-b
            :expand (lambda (_ctx _forms)
                      '((push :b packlet-test-user-kw-order))))
          (packlet-define-keyword :packlet-a
            :expand (lambda (_ctx _forms)
                      '((push :a packlet-test-user-kw-order))))
          (eval
           '(packlet packlet-test-user-kw-order-feature
              :packlet-a first
              :packlet-b second))
          (should (equal (nreverse packlet-test-user-kw-order)
                         '(:a :b))))
      (setq packlet-test-user-kw-order nil))))

(ert-deftest packlet-test-user-keyword-rejects-builtin ()
  (should-error (packlet-define-keyword :init
                  :expand #'ignore)
                :type 'error))

(ert-deftest packlet-test-delayed-hook-schedules-idle-timer ()
  "Delayed hook entries should schedule an idle timer instead of calling directly."
  (let* ((directory (make-temp-file "packlet-test-" t))
         (load-path (cons directory load-path))
         (scheduled nil)
         (timer-id 0))
    (unwind-protect
        (progn
          (packlet-test--cleanup-feature 'packlet-test-delayed-hook-feature)
          (packlet-test--cleanup-symbols
           '(packlet-test-delayed-fn packlet-test-delayed-ran))
          (defvar packlet-test-delayed-schedule-hook nil)
          (setq packlet-test-delayed-schedule-hook nil)
          (packlet-test--write-feature
           directory
           'packlet-test-delayed-hook-feature
           "(defvar packlet-test-delayed-ran nil)\n\
(defun packlet-test-delayed-fn ()\n\
  (setq packlet-test-delayed-ran t))")
          (cl-letf (((symbol-function 'run-with-idle-timer)
                     (lambda (secs repeat fn &rest args)
                       (let ((timer (list :timer (cl-incf timer-id))))
                         (push (list :timer timer :secs secs :repeat repeat
                                     :fn fn :args args)
                               scheduled)
                         timer)))
                    ((symbol-function 'timerp)
                     (lambda (object)
                       (and (consp object)
                            (eq (car object) :timer)))))
            (eval
             `(packlet packlet-test-delayed-hook-feature
                :hook ((packlet-test-delayed-schedule-hook packlet-test-delayed-fn 2.0))))
            ;; Hook should not have run the function directly
            (run-hooks 'packlet-test-delayed-schedule-hook)
            (should (= (length scheduled) 1))
            (should (= (plist-get (car scheduled) :secs) 2.0))
            ;; Simulate the idle timer firing
            (packlet-test--invoke-scheduled-timer (car scheduled))
            (should (bound-and-true-p packlet-test-delayed-ran))))
      (packlet-test--cleanup-feature 'packlet-test-delayed-hook-feature)
      (packlet-test--cleanup-symbols
       '(packlet-test-delayed-fn packlet-test-delayed-ran
         packlet-test-delayed-schedule-hook))
      (delete-directory directory t))))

(ert-deftest packlet-test-delayed-hook-debounces ()
  "Delayed hook should cancel previous timer when hook fires again."
  (let* ((scheduled nil)
         (cancelled nil)
         (timer-id 0))
    (defvar packlet-test-debounce-hook nil)
    (setq packlet-test-debounce-hook nil)
    (unwind-protect
        (cl-letf (((symbol-function 'run-with-idle-timer)
                   (lambda (secs repeat fn &rest args)
                     (let ((timer (list :timer (cl-incf timer-id))))
                       (push (list :timer timer :secs secs :repeat repeat
                                   :fn fn :args args)
                             scheduled)
                       timer)))
                  ((symbol-function 'timerp)
                   (lambda (object)
                     (and (consp object)
                          (eq (car object) :timer))))
                  ((symbol-function 'cancel-timer)
                   (lambda (timer)
                     (push timer cancelled))))
          (eval
           `(packlet packlet-test-debounce-feature
              :hook ((packlet-test-debounce-hook ignore 1.0))))
          ;; Fire hook twice
          (run-hooks 'packlet-test-debounce-hook)
          (should (= (length scheduled) 1))
          (run-hooks 'packlet-test-debounce-hook)
          ;; Second call should have cancelled the first timer
          (should (= (length cancelled) 1))
          (should (= (length scheduled) 2)))
      (packlet-test--cleanup-symbols '(packlet-test-debounce-hook)))))

(ert-deftest packlet-test-hook-reeval-does-not-duplicate ()
  (defvar packlet-test-reeval-hook nil)
  (unwind-protect
      (progn
        (setq packlet-test-hook-count 0
              packlet-test-reeval-hook nil)
        (eval
         '(packlet packlet-test-reeval-hook-feature
            :hook ((packlet-test-reeval-hook
                    . (lambda ()
                        (setq packlet-test-hook-count
                              (1+ packlet-test-hook-count)))))))
        (eval
         '(packlet packlet-test-reeval-hook-feature
            :hook ((packlet-test-reeval-hook
                    . (lambda ()
                        (setq packlet-test-hook-count
                              (1+ packlet-test-hook-count)))))))
        (run-hooks 'packlet-test-reeval-hook)
        (should (= packlet-test-hook-count 1)))
    (setq packlet-test-hook-count nil)
    (packlet-test--cleanup-symbols '(packlet-test-reeval-hook))))

(ert-deftest packlet-test-hook-reeval-after-entry-removal-clears-old-hook ()
  (defvar packlet-test-reeval-hook-a nil)
  (defvar packlet-test-reeval-hook-b nil)
  (let ((source-file (make-temp-file "packlet-test-hook-remove-" nil ".el")))
    (unwind-protect
        (progn
          (setq packlet-test-hook-count 0
                packlet-test-reeval-hook-a nil
                packlet-test-reeval-hook-b nil)
          (with-temp-buffer
            (emacs-lisp-mode)
            (setq buffer-file-name source-file)
            (insert "(packlet packlet-test-hook-remove-feature\n\
  :hook ((packlet-test-reeval-hook-a\n\
          . (lambda ()\n\
              (setq packlet-test-hook-count (1+ packlet-test-hook-count))))\n\
         (packlet-test-reeval-hook-b\n\
          . (lambda ()\n\
              (setq packlet-test-hook-count (+ packlet-test-hook-count 10))))))\n")
            (goto-char (point-min))
            (eval-buffer)
            (erase-buffer)
            (insert "(packlet packlet-test-hook-remove-feature\n\
  :hook ((packlet-test-reeval-hook-a\n\
          . (lambda ()\n\
              (setq packlet-test-hook-count (1+ packlet-test-hook-count))))))\n")
            (goto-char (point-min))
            (eval-buffer))
          (run-hooks 'packlet-test-reeval-hook-a)
          (run-hooks 'packlet-test-reeval-hook-b)
          (should (= packlet-test-hook-count 1))
          (should-not packlet-test-reeval-hook-b))
      (setq packlet-test-hook-count nil)
      (packlet-test--cleanup-symbols
       '(packlet-test-reeval-hook-a packlet-test-reeval-hook-b))
      (when (file-exists-p source-file)
        (delete-file source-file)))))

(ert-deftest packlet-test-bind-reeval-after-removal-preserves-later-global-binding ()
  (let* ((source-file (make-temp-file "packlet-test-bind-remove-" nil ".el"))
         (key (kbd "C-c z"))
         (old-binding (lookup-key global-map key)))
    (unwind-protect
        (progn
          (if old-binding
              (global-set-key key old-binding)
            (global-unset-key key))
          (with-temp-buffer
            (emacs-lisp-mode)
            (setq buffer-file-name source-file)
            (insert "(packlet packlet-test-bind-remove-feature\n\
  :bind (\"C-c z\" . ignore))\n")
            (goto-char (point-min))
            (eval-buffer))
          (global-set-key key 'forward-char)
          (with-temp-buffer
            (emacs-lisp-mode)
            (setq buffer-file-name source-file)
            (insert ";; removed\n")
            (goto-char (point-min))
            (eval-buffer))
          (should (eq (lookup-key global-map key) 'forward-char)))
      (if old-binding
          (global-set-key key old-binding)
        (global-unset-key key))
      (when (file-exists-p source-file)
        (delete-file source-file)))))

(ert-deftest packlet-test-delayed-hook-reeval-does-not-duplicate ()
  (let* ((scheduled nil)
         (timer-id 0))
    (defvar packlet-test-delayed-reeval-hook nil)
    (unwind-protect
        (progn
          (setq packlet-test-hook-count 0
                packlet-test-delayed-reeval-hook nil)
          (cl-letf (((symbol-function 'run-with-idle-timer)
                     (lambda (secs repeat fn &rest args)
                       (let ((timer (list :timer (cl-incf timer-id))))
                         (push (list :timer timer :secs secs :repeat repeat
                                     :fn fn :args args)
                               scheduled)
                         timer)))
                    ((symbol-function 'timerp)
                     (lambda (object)
                       (and (consp object)
                            (eq (car object) :timer)))))
            (eval
             '(packlet packlet-test-delayed-reeval-feature
                :hook ((packlet-test-delayed-reeval-hook
                        (lambda ()
                          (setq packlet-test-hook-count
                                (1+ packlet-test-hook-count)))
                        0.5))))
            (eval
             '(packlet packlet-test-delayed-reeval-feature
                :hook ((packlet-test-delayed-reeval-hook
                        (lambda ()
                          (setq packlet-test-hook-count
                                (1+ packlet-test-hook-count)))
                        0.5))))
            (run-hooks 'packlet-test-delayed-reeval-hook)
            (should (= (length scheduled) 1))
            (packlet-test--invoke-scheduled-timer (car scheduled))
            (should (= packlet-test-hook-count 1))))
      (setq packlet-test-hook-count nil)
      (packlet-test--cleanup-symbols '(packlet-test-delayed-reeval-hook)))))

(ert-deftest packlet-test-delayed-hook-rejects-negative-delay ()
  (should-error
   (eval
    '(packlet packlet-test-negative-delay
       :hook ((packlet-test-negative-delay-hook ignore -1.0))))
   :type 'error))

;;; packlet-test.el ends here
