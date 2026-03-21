;;; packlet-test.el --- Tests for packlet -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for `packlet'.

;;; Code:

(require 'ert)
(require 'packlet)

(defvar packlet-test-command-ran)
(defvar packlet-test-hook-ran)

(declare-function packlet--configure-packlet-test-feature "packlet-test" ())
(declare-function packlet-test-hook "packlet-test-feature" ())

(defun packlet-test--write-feature (directory feature body)
  "Write FEATURE with BODY into DIRECTORY."
  (with-temp-file (expand-file-name (format "%s.el" feature) directory)
    (insert (format ";;; %s.el --- Generated test feature -*- lexical-binding: t; -*-\n\n"
                    feature))
    (insert body)
    (unless (string-suffix-p "\n" body)
      (insert "\n"))
    (insert (format "(provide '%s)\n" feature))))

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
          (dolist (symbol '(packlet-test-command
                            packlet-test-hook
                            packlet-test-command-ran
                            packlet-test-hook-ran))
            (when (fboundp symbol)
              (fmakunbound symbol))
            (when (boundp symbol)
              (makunbound symbol)))
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
      (dolist (symbol '(packlet-test-command
                        packlet-test-hook
                        packlet-test-command-ran
                        packlet-test-hook-ran))
        (when (fboundp symbol)
          (fmakunbound symbol))
        (when (boundp symbol)
          (makunbound symbol)))
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

;;; packlet-test.el ends here
