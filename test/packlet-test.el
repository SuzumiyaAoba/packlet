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

(declare-function packlet--configure-packlet-test-feature "packlet-test" ())
(declare-function packlet-test-hook "packlet-test-feature" ())

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
  (let ((configured-var (intern (format "packlet--configured-%s" feature)))
        (configure-fn (intern (format "packlet--configure-%s" feature))))
    (when (boundp configured-var)
      (makunbound configured-var))
    (when (fboundp configure-fn)
      (fmakunbound configure-fn))))

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

(ert-deftest packlet-test-idle-keyword-registers-loader ()
  (let (calls)
    (cl-letf (((symbol-function 'packlet--register-idle-load)
               (lambda (feature afters file delay)
                 (push (list feature afters file delay) calls))))
      (packlet packlet-test-idle-default
        :idle)
      (packlet packlet-test-idle-delayed
        :after packlet-test-after
        :idle 2.5))
    (should
     (equal (nreverse calls)
            '((packlet-test-idle-default nil "packlet-test-idle-default" 1.0)
              (packlet-test-idle-delayed
               (packlet-test-after)
               "packlet-test-idle-delayed"
               2.5))))))

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
      (packlet--register-idle-load 'packlet-test-idle nil "packlet-test-idle" 2.5)
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
      (packlet--register-idle-load 'packlet-test-idle nil "packlet-test-idle" 1.0)
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
         (packlet-directory (file-name-directory (locate-library "packlet"))))
    (unwind-protect
        (progn
          (packlet-test--write-library
           directory
           library-file
           library-feature
           "(defvar packlet-test-compile-variable nil)\n\
(defun packlet-test-compile-function ()\n\
  packlet-test-compile-variable)")
          (packlet-test--write-file
           config-file
           ";;; packlet-test-config.el --- Generated test config -*- lexical-binding: t; -*-\n\n\
(require 'packlet)\n\n\
(packlet packlet-test-compile-feature\n\
  :file packlet-test-compile-library\n\
  :defines packlet-test-compile-variable\n\
  :functions packlet-test-compile-function\n\
  :custom (packlet-test-compile-variable 42)\n\
  :config\n\
  (when packlet-test-compile-variable\n\
    (packlet-test-compile-function)))")
          (let ((load-path (append (list directory packlet-directory) load-path))
                (byte-compile-error-on-warn t))
            (should (byte-compile-file config-file))
            (should (file-exists-p compiled-file))))
      (when (file-exists-p compiled-file)
        (delete-file compiled-file))
      (delete-directory directory t))))

;;; packlet-test.el ends here
