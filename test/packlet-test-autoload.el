;;; packlet-test-autoload.el --- Autoload and mode dispatch tests -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Tests for autoloads, interpreter dispatch, and magic associations.

;;; Code:

(eval-and-compile
  (require 'packlet-test-support))

(defvar packlet-test-custom-value)

(declare-function packlet-test-hook "packlet-test-feature" ())
(declare-function packlet-test-al-custom-file "packlet-test-al-lib" ())

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

(ert-deftest packlet-test-interpreter-autoload-and-registration ()
  (let* ((directory (make-temp-file "packlet-test-" t))
         (load-path (cons directory load-path))
         (interpreter-mode-alist interpreter-mode-alist)
         (feature 'packlet-test-interpreter-feature)
         (symbols '(packlet-test-script-mode
                    packlet-test-interpreter-ran)))
    (unwind-protect
        (progn
          (packlet-test--cleanup-feature feature)
          (packlet-test--cleanup-symbols symbols)
          (packlet-test--write-feature
           directory
           feature
           "(defvar packlet-test-interpreter-ran nil)\n\
(defun packlet-test-script-mode ()\n\
  (interactive)\n\
  (setq packlet-test-interpreter-ran t)\n\
  (setq major-mode 'packlet-test-script-mode))")
          (packlet packlet-test-interpreter-feature
            :interpreter ("packlet-test-script" . packlet-test-script-mode))
          (should (autoloadp (symbol-function 'packlet-test-script-mode)))
          (should (equal (assoc "packlet-test-script" interpreter-mode-alist)
                         '("packlet-test-script" . packlet-test-script-mode)))
          (with-temp-buffer
            (setq buffer-file-name (expand-file-name "script" directory))
            (insert "#!/usr/bin/env packlet-test-script\n")
            (set-auto-mode)
            (should (eq major-mode 'packlet-test-script-mode))
            (should (bound-and-true-p packlet-test-interpreter-ran))))
      (packlet-test--cleanup-feature feature)
      (packlet-test--cleanup-symbols symbols)
      (delete-directory directory t))))

(ert-deftest packlet-test-interpreter-reeval-after-removal-clears-old-entry ()
  (let ((source-file (make-temp-file "packlet-test-interpreter-remove-" nil ".el"))
        (interpreter-mode-alist interpreter-mode-alist))
    (unwind-protect
        (progn
          (packlet-test--cleanup-symbols '(packlet-test-interpreter-remove-mode))
          (with-temp-buffer
            (emacs-lisp-mode)
            (setq buffer-file-name source-file)
            (insert "(packlet packlet-test-interpreter-remove-feature\n\
  :interpreter (\"packlet-test-remove\" . packlet-test-interpreter-remove-mode))\n")
            (goto-char (point-min))
            (eval-buffer)
            (should (equal (assoc "packlet-test-remove" interpreter-mode-alist)
                           '("packlet-test-remove"
                             . packlet-test-interpreter-remove-mode)))
            (erase-buffer)
            (insert ";; removed\n")
            (goto-char (point-min))
            (eval-buffer))
          (should-not (assoc "packlet-test-remove" interpreter-mode-alist)))
      (packlet-test--cleanup-symbols '(packlet-test-interpreter-remove-mode))
      (when (file-exists-p source-file)
        (delete-file source-file)))))

(ert-deftest packlet-test-magic-autoload-and-registration ()
  (let* ((directory (make-temp-file "packlet-test-" t))
         (load-path (cons directory load-path))
         (magic-mode-alist magic-mode-alist)
         (feature 'packlet-test-magic-feature)
         (symbols '(packlet-test-magic-mode
                    packlet-test-magic-ran)))
    (unwind-protect
        (progn
          (packlet-test--cleanup-feature feature)
          (packlet-test--cleanup-symbols symbols)
          (packlet-test--write-feature
           directory
           feature
           "(defvar packlet-test-magic-ran nil)\n\
(defun packlet-test-magic-mode ()\n\
  (interactive)\n\
  (setq packlet-test-magic-ran t)\n\
  (setq major-mode 'packlet-test-magic-mode))")
          (packlet packlet-test-magic-feature
            :magic ("PACKLET-MAGIC\\'" . packlet-test-magic-mode))
          (should (autoloadp (symbol-function 'packlet-test-magic-mode)))
          (should (equal (assoc "PACKLET-MAGIC\\'" magic-mode-alist)
                         '("PACKLET-MAGIC\\'" . packlet-test-magic-mode)))
          (with-temp-buffer
            (setq buffer-file-name (expand-file-name "magic" directory))
            (insert "PACKLET-MAGIC")
            (set-auto-mode)
            (should (eq major-mode 'packlet-test-magic-mode))
            (should (bound-and-true-p packlet-test-magic-ran))))
      (packlet-test--cleanup-feature feature)
      (packlet-test--cleanup-symbols symbols)
      (delete-directory directory t))))

(ert-deftest packlet-test-magic-reeval-after-removal-clears-old-entry ()
  (let ((source-file (make-temp-file "packlet-test-magic-remove-" nil ".el"))
        (magic-mode-alist magic-mode-alist))
    (unwind-protect
        (progn
          (packlet-test--cleanup-symbols '(packlet-test-magic-remove-mode))
          (with-temp-buffer
            (emacs-lisp-mode)
            (setq buffer-file-name source-file)
            (insert "(packlet packlet-test-magic-remove-feature\n\
  :magic (\"PACKLET-REMOVE\\\\'\" . packlet-test-magic-remove-mode))\n")
            (goto-char (point-min))
            (eval-buffer)
            (should (equal (assoc "PACKLET-REMOVE\\'" magic-mode-alist)
                           '("PACKLET-REMOVE\\'"
                             . packlet-test-magic-remove-mode)))
            (erase-buffer)
            (insert ";; removed\n")
            (goto-char (point-min))
            (eval-buffer))
          (should-not (assoc "PACKLET-REMOVE\\'" magic-mode-alist)))
      (packlet-test--cleanup-symbols '(packlet-test-magic-remove-mode))
      (when (file-exists-p source-file)
        (delete-file source-file)))))

(ert-deftest packlet-test-magic-fallback-autoload-and-registration ()
  (let* ((directory (make-temp-file "packlet-test-" t))
         (load-path (cons directory load-path))
         (magic-fallback-mode-alist magic-fallback-mode-alist)
         (feature 'packlet-test-magic-fallback-feature)
         (symbols '(packlet-test-magic-fallback-mode
                    packlet-test-magic-fallback-ran)))
    (unwind-protect
        (progn
          (packlet-test--cleanup-feature feature)
          (packlet-test--cleanup-symbols symbols)
          (packlet-test--write-feature
           directory
           feature
           "(defvar packlet-test-magic-fallback-ran nil)\n\
(defun packlet-test-magic-fallback-mode ()\n\
  (interactive)\n\
  (setq packlet-test-magic-fallback-ran t)\n\
  (setq major-mode 'packlet-test-magic-fallback-mode))")
          (packlet packlet-test-magic-fallback-feature
            :magic-fallback ("PACKLET-FALLBACK\\'" . packlet-test-magic-fallback-mode))
          (should (autoloadp (symbol-function 'packlet-test-magic-fallback-mode)))
          (should (equal (assoc "PACKLET-FALLBACK\\'" magic-fallback-mode-alist)
                         '("PACKLET-FALLBACK\\'" . packlet-test-magic-fallback-mode)))
          (with-temp-buffer
            (setq buffer-file-name (expand-file-name "fallback" directory))
            (insert "PACKLET-FALLBACK")
            (set-auto-mode)
            (should (eq major-mode 'packlet-test-magic-fallback-mode))
            (should (bound-and-true-p packlet-test-magic-fallback-ran))))
      (packlet-test--cleanup-feature feature)
      (packlet-test--cleanup-symbols symbols)
      (delete-directory directory t))))

(ert-deftest packlet-test-magic-fallback-reeval-after-removal-clears-old-entry ()
  (let ((source-file (make-temp-file "packlet-test-magic-fallback-remove-" nil ".el"))
        (magic-fallback-mode-alist magic-fallback-mode-alist))
    (unwind-protect
        (progn
          (packlet-test--cleanup-symbols
           '(packlet-test-magic-fallback-remove-mode))
          (with-temp-buffer
            (emacs-lisp-mode)
            (setq buffer-file-name source-file)
            (insert "(packlet packlet-test-magic-fallback-remove-feature\n\
  :magic-fallback (\"PACKLET-FALLBACK-REMOVE\\\\'\" . packlet-test-magic-fallback-remove-mode))\n")
            (goto-char (point-min))
            (eval-buffer)
            (should (equal (assoc "PACKLET-FALLBACK-REMOVE\\'"
                                  magic-fallback-mode-alist)
                           '("PACKLET-FALLBACK-REMOVE\\'"
                             . packlet-test-magic-fallback-remove-mode)))
            (erase-buffer)
            (insert ";; removed\n")
            (goto-char (point-min))
            (eval-buffer))
          (should-not (assoc "PACKLET-FALLBACK-REMOVE\\'"
                             magic-fallback-mode-alist)))
      (packlet-test--cleanup-symbols
       '(packlet-test-magic-fallback-remove-mode))
      (when (file-exists-p source-file)
        (delete-file source-file)))))

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
          (packlet-test--write-feature
           directory
           feature
           "(defvar packlet-test-al-simple-ran nil)\n\
(defun packlet-test-al-simple ()\n\
  (setq packlet-test-al-simple-ran t))")
          (packlet-test--write-library
           directory
           'packlet-test-al-lib
           'packlet-test-al-lib
           "(defvar packlet-test-al-custom-ran nil)\n\
(defun packlet-test-al-custom-file ()\n\
  (setq packlet-test-al-custom-ran t))")
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
          (should (autoloadp (symbol-function 'packlet-test-al-simple)))
          (should (equal (nth 1 (symbol-function 'packlet-test-al-simple))
                         "packlet-test-autoload-extended"))
          (should-not (nth 3 (symbol-function 'packlet-test-al-simple)))
          (should (autoloadp (symbol-function 'packlet-test-al-custom-file)))
          (should (equal (nth 1 (symbol-function 'packlet-test-al-custom-file))
                         "packlet-test-al-lib"))
          (should-not (nth 3 (symbol-function 'packlet-test-al-custom-file)))
          (should (autoloadp (symbol-function 'packlet-test-al-interactive)))
          (should (equal (nth 1 (symbol-function 'packlet-test-al-interactive))
                         "packlet-test-al-cmd"))
          (should (nth 3 (symbol-function 'packlet-test-al-interactive)))
          (funcall 'packlet-test-al-custom-file)
          (should (bound-and-true-p packlet-test-al-custom-ran))
          (call-interactively 'packlet-test-al-interactive)
          (should (bound-and-true-p packlet-test-al-interactive-ran)))
      (packlet-test--cleanup-feature feature)
      (packlet-test--cleanup-feature 'packlet-test-al-lib)
      (packlet-test--cleanup-feature 'packlet-test-al-cmd)
      (packlet-test--cleanup-symbols symbols)
      (delete-directory directory t))))

(provide 'packlet-test-autoload)

;;; packlet-test-autoload.el ends here
