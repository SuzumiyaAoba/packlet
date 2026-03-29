;;; packlet-test-bindings.el --- Binding tests for packlet -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Tests for key bindings, keymaps, and prefix maps.

;;; Code:

(eval-and-compile
  (require 'packlet-test-support))

(defvar packlet-test-parent-map)

(ert-deftest packlet-test-bind-keymap-loads-prefix-map ()
  (let* ((directory (make-temp-file "packlet-test-" t))
         (load-path (cons directory load-path))
         (feature 'packlet-test-prefix-feature)
         (symbols '(packlet-test-prefix-map
                    packlet-test-prefix-command
                    packlet-test-prefix-command-ran))
         (key (kbd "C-c C-k"))
         (old-binding (lookup-key global-map key)))
    (unwind-protect
        (progn
          (packlet-test--cleanup-feature feature)
          (packlet-test--cleanup-symbols symbols)
          (packlet-test--write-feature
           directory
           feature
           "(defvar packlet-test-prefix-command-ran nil)\n\
(defvar packlet-test-prefix-map (make-sparse-keymap))\n\
(defun packlet-test-prefix-command ()\n\
  (interactive)\n\
  (setq packlet-test-prefix-command-ran t))\n\
(define-key packlet-test-prefix-map (kbd \"a\") #'packlet-test-prefix-command)")
          (eval
           `(packlet ,feature
              :bind-keymap ("C-c C-k" . packlet-test-prefix-map)))
          (should-not (featurep feature))
          (should (commandp (lookup-key global-map key)))
          (execute-kbd-macro (kbd "C-c C-k a"))
          (should (featurep feature))
          (should (bound-and-true-p packlet-test-prefix-command-ran))
          (should (keymapp (lookup-key global-map key))))
      (packlet-test--cleanup-feature feature)
      (packlet-test--cleanup-symbols symbols)
      (if old-binding
          (global-set-key key old-binding)
        (global-unset-key key))
      (delete-directory directory t))))

(ert-deftest packlet-test-bind-keymap-map-group-loads-prefix-map ()
  (let* ((directory (make-temp-file "packlet-test-" t))
         (load-path (cons directory load-path))
         (parent-feature 'packlet-test-prefix-parent)
         (feature 'packlet-test-prefix-map-feature)
         (symbols '(packlet-test-parent-map
                    packlet-test-child-map
                    packlet-test-prefix-map-command
                    packlet-test-prefix-map-command-ran)))
    (unwind-protect
        (progn
          (packlet-test--cleanup-feature parent-feature)
          (packlet-test--cleanup-feature feature)
          (packlet-test--cleanup-symbols symbols)
          (packlet-test--write-feature
           directory
           parent-feature
           "(defvar packlet-test-parent-map (make-sparse-keymap))")
          (packlet-test--write-feature
           directory
           feature
           "(defvar packlet-test-prefix-map-command-ran nil)\n\
(defvar packlet-test-child-map (make-sparse-keymap))\n\
(defun packlet-test-prefix-map-command ()\n\
  (interactive)\n\
  (setq packlet-test-prefix-map-command-ran t))\n\
(define-key packlet-test-child-map (kbd \"b\") #'packlet-test-prefix-map-command)")
          (eval
           `(packlet ,feature
              :after ,parent-feature
              :bind-keymap (:map packlet-test-parent-map
                             ("C-c m" . packlet-test-child-map))))
          (should-not (featurep feature))
          (require parent-feature)
          (let ((loader (lookup-key packlet-test-parent-map (kbd "C-c m"))))
            (should (commandp loader))
            (call-interactively loader))
          (should (featurep feature))
          (call-interactively
           (lookup-key (lookup-key packlet-test-parent-map (kbd "C-c m"))
                       (kbd "b")))
          (should (bound-and-true-p packlet-test-prefix-map-command-ran))
          (should (keymapp (lookup-key packlet-test-parent-map
                                       (kbd "C-c m")))))
      (packlet-test--cleanup-feature parent-feature)
      (packlet-test--cleanup-feature feature)
      (packlet-test--cleanup-symbols symbols)
      (delete-directory directory t))))

(ert-deftest packlet-test-bind-keymap-reeval-after-removal-restores-old-binding ()
  (let* ((directory (make-temp-file "packlet-test-" t))
         (load-path (cons directory load-path))
         (feature 'packlet-test-prefix-reeval-feature)
         (source-file (expand-file-name "packlet-test-prefix-reeval.el"
                                        directory))
         (key (kbd "C-c C-r"))
         (old-binding (lookup-key global-map key)))
    (unwind-protect
        (progn
          (packlet-test--cleanup-feature feature)
          (packlet-test--cleanup-symbols
           '(packlet-test-prefix-reeval-map
             packlet-test-prefix-command
             packlet-test-prefix-command-ran))
          (packlet-test--write-feature
           directory
           feature
           "(defvar packlet-test-prefix-command-ran nil)\n\
(defvar packlet-test-prefix-reeval-map (make-sparse-keymap))\n\
(defun packlet-test-prefix-command ()\n\
  (interactive)\n\
  (setq packlet-test-prefix-command-ran t))\n\
(define-key packlet-test-prefix-reeval-map (kbd \"a\") #'packlet-test-prefix-command)")
          (with-temp-buffer
            (emacs-lisp-mode)
            (setq buffer-file-name source-file)
            (insert "(packlet packlet-test-prefix-reeval-feature\n\
  :bind-keymap (\"C-c C-r\" . packlet-test-prefix-reeval-map))\n")
            (goto-char (point-min))
            (eval-buffer))
          (execute-kbd-macro (kbd "C-c C-r a"))
          (should (keymapp (lookup-key global-map key)))
          (with-temp-buffer
            (emacs-lisp-mode)
            (setq buffer-file-name source-file)
            (insert ";; removed\n")
            (goto-char (point-min))
            (eval-buffer))
          (should (equal (lookup-key global-map key) old-binding)))
      (packlet-test--cleanup-feature feature)
      (packlet-test--cleanup-symbols
       '(packlet-test-prefix-reeval-map
         packlet-test-prefix-command
         packlet-test-prefix-command-ran))
      (if old-binding
          (global-set-key key old-binding)
        (global-unset-key key))
      (delete-directory directory t))))

(ert-deftest packlet-test-prefix-map-keyword-creates-and-loads-prefix-map ()
  (let* ((directory (make-temp-file "packlet-test-" t))
         (load-path (cons directory load-path))
         (feature 'packlet-test-prefix-keyword-feature)
         (symbols '(packlet-test-prefix-keyword-map
                    packlet-test-prefix-keyword-command
                    packlet-test-prefix-keyword-ran))
         (key (kbd "C-c C-p"))
         (old-binding (lookup-key global-map key)))
    (unwind-protect
        (progn
          (packlet-test--cleanup-feature feature)
          (packlet-test--cleanup-symbols symbols)
          (packlet-test--write-feature
           directory
           feature
           "(defvar packlet-test-prefix-keyword-ran nil)\n\
(defun packlet-test-prefix-keyword-command ()\n\
  (interactive)\n\
  (setq packlet-test-prefix-keyword-ran t))")
          (eval
           `(packlet ,feature
              :commands (packlet-test-prefix-keyword-command)
              :prefix-map packlet-test-prefix-keyword-map
              :bind-keymap ("C-c C-p" . packlet-test-prefix-keyword-map)
              :bind (:map packlet-test-prefix-keyword-map
                     ("a" . packlet-test-prefix-keyword-command))))
          (should (boundp 'packlet-test-prefix-keyword-map))
          (should (keymapp packlet-test-prefix-keyword-map))
          (should-not (featurep feature))
          (execute-kbd-macro (kbd "C-c C-p a"))
          (should (featurep feature))
          (should (bound-and-true-p packlet-test-prefix-keyword-ran)))
      (packlet-test--cleanup-feature feature)
      (packlet-test--cleanup-symbols symbols)
      (if old-binding
          (global-set-key key old-binding)
        (global-unset-key key))
      (delete-directory directory t))))

(ert-deftest packlet-test-prefix-map-reeval-after-removal-unbinds-created-map ()
  (let ((source-file (make-temp-file "packlet-test-prefix-map-remove-" nil ".el")))
    (unwind-protect
        (progn
          (packlet-test--cleanup-symbols '(packlet-test-prefix-map-remove-map))
          (with-temp-buffer
            (emacs-lisp-mode)
            (setq buffer-file-name source-file)
            (insert "(packlet packlet-test-prefix-map-remove-feature\n\
  :prefix-map packlet-test-prefix-map-remove-map)\n")
            (goto-char (point-min))
            (eval-buffer))
          (should (boundp 'packlet-test-prefix-map-remove-map))
          (should (keymapp packlet-test-prefix-map-remove-map))
          (with-temp-buffer
            (emacs-lisp-mode)
            (setq buffer-file-name source-file)
            (insert ";; removed\n")
            (goto-char (point-min))
            (eval-buffer))
          (should-not (boundp 'packlet-test-prefix-map-remove-map)))
      (packlet-test--cleanup-symbols '(packlet-test-prefix-map-remove-map))
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

(provide 'packlet-test-bindings)

;;; packlet-test-bindings.el ends here
