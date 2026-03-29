;;; packlet-test-keywords.el --- Keyword behavior tests for packlet -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Tests for keyword-specific behavior in `packlet'.

;;; Code:

(eval-and-compile
  (require 'packlet-test-support))

(defvar packlet-test-compile-variable)
(defvar packlet-test-compile-function-ran)

(declare-function packlet-test-advised-target nil ())
(declare-function packlet-test-enable-mode nil (&optional arg))
(declare-function packlet-test-derived-mode nil ())

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

(ert-deftest packlet-test-setq-keyword ()
  (let ((packlet-test-setq-a nil)
        (packlet-test-setq-b nil))
    (packlet packlet-test-setq-feature
      :setq ((packlet-test-setq-a 42)
             (packlet-test-setq-b "hello")))
    (should (= packlet-test-setq-a 42))
    (should (equal packlet-test-setq-b "hello"))))

(ert-deftest packlet-test-settings-reeval-removal-restores-previous-values ()
  (let ((source-file (make-temp-file "packlet-test-settings-remove-" nil ".el")))
    (unwind-protect
        (progn
          (setq packlet-test-tracked-setq 10
                packlet-test-tracked-custom 20)
          (with-temp-buffer
            (emacs-lisp-mode)
            (setq buffer-file-name source-file)
            (insert "(packlet packlet-test-settings-remove-feature\n\
  :setq (packlet-test-tracked-setq 1)\n\
  :custom (packlet-test-tracked-custom 2))\n")
            (goto-char (point-min))
            (eval-buffer)
            (should (= packlet-test-tracked-setq 1))
            (should (= packlet-test-tracked-custom 2))
            (erase-buffer)
            (insert ";; removed\n")
            (goto-char (point-min))
            (eval-buffer))
          (should (= packlet-test-tracked-setq 10))
          (should (= packlet-test-tracked-custom 20)))
      (ignore-errors
        (with-temp-buffer
          (emacs-lisp-mode)
          (setq buffer-file-name source-file)
          (insert ";; removed\n")
          (goto-char (point-min))
          (eval-buffer)))
      (setq packlet-test-tracked-setq 0
            packlet-test-tracked-custom 0)
      (when (file-exists-p source-file)
        (delete-file source-file)))))

(ert-deftest packlet-test-settings-reeval-error-rolls-back-old-values ()
  (let ((source-file (make-temp-file "packlet-test-settings-rollback-" nil ".el")))
    (unwind-protect
        (progn
          (setq packlet-test-tracked-setq 10
                packlet-test-tracked-custom 20)
          (with-temp-buffer
            (emacs-lisp-mode)
            (setq buffer-file-name source-file)
            (insert "(packlet packlet-test-settings-rollback-feature\n\
  :setq (packlet-test-tracked-setq 1)\n\
  :custom (packlet-test-tracked-custom 2))\n")
            (goto-char (point-min))
            (eval-buffer)
            (erase-buffer)
            (insert "(packlet packlet-test-settings-rollback-feature\n\
  :setq (packlet-test-tracked-setq 3)\n\
  :custom (packlet-test-tracked-custom 4))\n\
(this-symbol-does-not-exist)\n")
            (goto-char (point-min))
            (should-error (eval-buffer)))
          (should (= packlet-test-tracked-setq 1))
          (should (= packlet-test-tracked-custom 2)))
      (ignore-errors
        (with-temp-buffer
          (emacs-lisp-mode)
          (setq buffer-file-name source-file)
          (insert ";; removed\n")
          (goto-char (point-min))
          (eval-buffer)))
      (setq packlet-test-tracked-setq 0
            packlet-test-tracked-custom 0)
      (when (file-exists-p source-file)
        (delete-file source-file)))))

(ert-deftest packlet-test-settings-removal-preserves-later-user-change ()
  (let ((source-file (make-temp-file "packlet-test-settings-preserve-" nil ".el")))
    (unwind-protect
        (progn
          (setq packlet-test-tracked-setq 10
                packlet-test-tracked-custom 20)
          (with-temp-buffer
            (emacs-lisp-mode)
            (setq buffer-file-name source-file)
            (insert "(packlet packlet-test-settings-preserve-feature\n\
  :setq (packlet-test-tracked-setq 1)\n\
  :custom (packlet-test-tracked-custom 2))\n")
            (goto-char (point-min))
            (eval-buffer))
          (setq packlet-test-tracked-setq 99
                packlet-test-tracked-custom 98)
          (with-temp-buffer
            (emacs-lisp-mode)
            (setq buffer-file-name source-file)
            (insert ";; removed\n")
            (goto-char (point-min))
            (eval-buffer))
          (should (= packlet-test-tracked-setq 99))
          (should (= packlet-test-tracked-custom 98)))
      (ignore-errors
        (with-temp-buffer
          (emacs-lisp-mode)
          (setq buffer-file-name source-file)
          (insert ";; removed\n")
          (goto-char (point-min))
          (eval-buffer)))
      (setq packlet-test-tracked-setq 0
            packlet-test-tracked-custom 0)
      (when (file-exists-p source-file)
        (delete-file source-file)))))

(ert-deftest packlet-test-add-to-list-keyword ()
  (let ((packlet-test-list-a '(2))
        (packlet-test-list-b '(1))
        (packlet-test-compare-list '(("Alpha" . 1))))
    (packlet packlet-test-add-to-list-feature
      :add-to-list ((packlet-test-list-a 1)
                    (packlet-test-list-b 2 :append t)
                    (packlet-test-compare-list
                     '("alpha" . 2)
                     :compare (lambda (left right)
                                (string-equal (downcase (car left))
                                              (downcase (car right)))))))
    (should (equal packlet-test-list-a '(1 2)))
    (should (equal packlet-test-list-b '(1 2)))
    (should (equal packlet-test-compare-list '(("Alpha" . 1))))))

(ert-deftest packlet-test-add-to-list-reeval-removal-preserves-later-change ()
  (let ((source-file (make-temp-file "packlet-test-add-to-list-remove-" nil ".el"))
        (packlet-test-managed-list '(2)))
    (unwind-protect
        (progn
          (with-temp-buffer
            (emacs-lisp-mode)
            (setq buffer-file-name source-file)
            (insert "(packlet packlet-test-add-to-list-remove-feature\n\
  :add-to-list (packlet-test-managed-list 1))\n")
            (goto-char (point-min))
            (eval-buffer))
          (should (equal packlet-test-managed-list '(1 2)))
          (setq packlet-test-managed-list '(99 1 2))
          (with-temp-buffer
            (emacs-lisp-mode)
            (setq buffer-file-name source-file)
            (insert ";; removed\n")
            (goto-char (point-min))
            (eval-buffer))
          (should (equal packlet-test-managed-list '(99 2))))
      (when (file-exists-p source-file)
        (delete-file source-file)))))

(ert-deftest packlet-test-list-keyword ()
  (let ((packlet-test-list-a '(3))
        (packlet-test-list-b '(1)))
    (with-temp-buffer
      (emacs-lisp-mode)
      (packlet packlet-test-list-feature
        :list ((packlet-test-list-a 1 2)
               (packlet-test-list-b 2 3 :append t)))
      (should (equal packlet-test-list-a '(1 2 3)))
      (should (equal packlet-test-list-b '(1 2 3))))))

(ert-deftest packlet-test-alist-keyword-compares-by-car ()
  (let ((packlet-test-alist '(("alpha" . 1))))
    (with-temp-buffer
      (emacs-lisp-mode)
      (packlet packlet-test-alist-feature
        :alist (packlet-test-alist '("alpha" . 2)))
      (should (equal packlet-test-alist '(("alpha" . 1)))))))

(ert-deftest packlet-test-enable-keyword ()
  (let* ((directory (make-temp-file "packlet-test-" t))
         (load-path (cons directory load-path))
         (feature 'packlet-test-enable-feature))
    (unwind-protect
        (progn
          (packlet-test--cleanup-feature feature)
          (packlet-test--cleanup-symbols '(packlet-test-enable-mode))
          (packlet-test--write-feature
           directory
           feature
           "(defvar packlet-test-enable-mode nil)\n\
(defun packlet-test-enable-mode (&optional arg)\n\
  (setq packlet-test-enable-mode\n\
        (if (memq arg '(0 -1 nil)) nil arg)))")
          (with-temp-buffer
            (emacs-lisp-mode)
            (eval
             `(packlet ,feature
                :enable (packlet-test-enable-mode 'deferred)
                :demand t))
            (should (featurep feature))
            (should (eq packlet-test-enable-mode 'deferred))))
      (packlet-test--cleanup-feature feature)
      (packlet-test--cleanup-symbols '(packlet-test-enable-mode))
      (delete-directory directory t))))

(ert-deftest packlet-test-enable-reeval-removal-preserves-later-change ()
  (let* ((directory (make-temp-file "packlet-test-" t))
         (load-path (cons directory load-path))
         (feature 'packlet-test-enable-remove-feature)
         (source-file (make-temp-file "packlet-test-enable-remove-" nil ".el")))
    (unwind-protect
        (progn
          (packlet-test--cleanup-feature feature)
          (packlet-test--cleanup-symbols '(packlet-test-enable-mode))
          (packlet-test--write-feature
           directory
           feature
           "(defvar packlet-test-enable-mode nil)\n\
(defun packlet-test-enable-mode (&optional arg)\n\
  (setq packlet-test-enable-mode\n\
        (if (memq arg '(0 -1 nil)) nil arg)))")
          (with-temp-buffer
            (emacs-lisp-mode)
            (setq buffer-file-name source-file)
            (insert (format "(packlet %S\n  :enable packlet-test-enable-mode\n  :demand t)\n"
                            feature))
            (goto-char (point-min))
            (eval-buffer))
          (should (eq packlet-test-enable-mode 1))
          (packlet-test-enable-mode 'manual)
          (with-temp-buffer
            (emacs-lisp-mode)
            (setq buffer-file-name source-file)
            (insert ";; removed\n")
            (goto-char (point-min))
            (eval-buffer))
          (should (eq packlet-test-enable-mode 'manual)))
      (packlet-test--cleanup-feature feature)
      (packlet-test--cleanup-symbols '(packlet-test-enable-mode))
      (when (file-exists-p source-file)
        (delete-file source-file))
      (delete-directory directory t))))

(ert-deftest packlet-test-remap-keyword ()
  (let* ((directory (make-temp-file "packlet-test-" t))
         (load-path (cons directory load-path))
         (feature 'packlet-test-remap-feature)
         (source-file (make-temp-file "packlet-test-remap-" nil ".el")))
    (unwind-protect
        (progn
          (packlet-test--cleanup-feature feature)
          (packlet-test--cleanup-symbols '(packlet-test-remap-target-mode))
          (packlet-test--write-feature
           directory
           feature
           "(define-derived-mode packlet-test-remap-target-mode fundamental-mode \"Remap\")")
          (with-temp-buffer
            (emacs-lisp-mode)
            (setq buffer-file-name source-file)
            (insert (format "(packlet %S\n  :remap (packlet-test-remap-source-mode . packlet-test-remap-target-mode))\n"
                            feature))
            (goto-char (point-min))
            (eval-buffer))
          (should (eq (alist-get 'packlet-test-remap-source-mode
                                 major-mode-remap-alist)
                      'packlet-test-remap-target-mode))
          (should (autoloadp (symbol-function 'packlet-test-remap-target-mode)))
          (with-temp-buffer
            (emacs-lisp-mode)
            (setq buffer-file-name source-file)
            (insert ";; removed\n")
            (goto-char (point-min))
            (eval-buffer))
          (should-not (assq 'packlet-test-remap-source-mode
                            major-mode-remap-alist)))
      (setq major-mode-remap-alist
            (assq-delete-all 'packlet-test-remap-source-mode
                             major-mode-remap-alist))
      (packlet-test--cleanup-feature feature)
      (packlet-test--cleanup-symbols '(packlet-test-remap-target-mode))
      (when (file-exists-p source-file)
        (delete-file source-file))
      (delete-directory directory t))))

(ert-deftest packlet-test-derived-mode-keyword ()
  (let ((source-file (make-temp-file "packlet-test-derived-mode-" nil ".el"))
        (symbols '(packlet-test-derived-mode
                   packlet-test-derived-mode-hook
                   packlet-test-derived-mode-map
                   packlet-test-derived-mode-syntax-table
                   packlet-test-derived-mode-abbrev-table)))
    (unwind-protect
        (progn
          (packlet-test--cleanup-symbols symbols)
          (with-temp-buffer
            (emacs-lisp-mode)
            (setq buffer-file-name source-file)
            (insert "(packlet packlet-test-derived-mode-feature\n\
  :derived-mode (packlet-test-derived-mode text-mode \"Derived\"))\n")
            (goto-char (point-min))
            (eval-buffer))
          (should (fboundp 'packlet-test-derived-mode))
          (with-temp-buffer
            (packlet-test-derived-mode)
            (should (eq major-mode 'packlet-test-derived-mode))
            (should (derived-mode-p 'text-mode)))
          (with-temp-buffer
            (emacs-lisp-mode)
            (setq buffer-file-name source-file)
            (insert ";; removed\n")
            (goto-char (point-min))
            (eval-buffer))
          (should-not (fboundp 'packlet-test-derived-mode))
          (should-not (boundp 'packlet-test-derived-mode-hook)))
      (packlet-test--cleanup-symbols symbols)
      (when (file-exists-p source-file)
        (delete-file source-file)))))

(ert-deftest packlet-test-faces-keyword ()
  (let* ((directory (make-temp-file "packlet-test-" t))
         (load-path (cons directory load-path))
         (feature 'packlet-test-faces-feature)
         (source-file (make-temp-file "packlet-test-faces-" nil ".el"))
         (source-face 'packlet-test-faces-source-face)
         (target-face 'packlet-test-faces-target-face)
         source-snapshot
         target-snapshot)
    (unwind-protect
        (progn
          (make-empty-face source-face)
          (make-empty-face target-face)
          (setq source-snapshot (packlet--face-snapshot source-face))
          (set-face-attribute source-face nil :foreground "red" :background "blue")
          (set-face-attribute target-face nil :foreground "white" :background "black" :height 100)
          (setq target-snapshot (packlet--face-snapshot target-face))
          (packlet-test--write-feature directory feature "")
          (with-temp-buffer
            (emacs-lisp-mode)
            (setq buffer-file-name source-file)
            (insert (format "(packlet %S\n  :faces ((%S :copy %S :height 140))\n  :demand t)\n"
                            feature target-face source-face))
            (goto-char (point-min))
            (eval-buffer))
          (should (featurep feature))
          (should (equal (face-attribute target-face :foreground nil 'default)
                         (face-attribute source-face :foreground nil 'default)))
          (should (equal (face-attribute target-face :background nil 'default)
                         (face-attribute source-face :background nil 'default)))
          (should (= (face-attribute target-face :height nil 'default) 140))
          (with-temp-buffer
            (emacs-lisp-mode)
            (setq buffer-file-name source-file)
            (insert ";; removed\n")
            (goto-char (point-min))
            (eval-buffer))
          (should (equal (face-attribute target-face :foreground nil 'default)
                         (alist-get :foreground target-snapshot nil nil #'eq)))
          (should (equal (face-attribute target-face :background nil 'default)
                         (alist-get :background target-snapshot nil nil #'eq)))
          (should (equal (face-attribute target-face :height nil 'default)
                         (alist-get :height target-snapshot nil nil #'eq))))
      (packlet-test--cleanup-feature feature)
      (packlet-test--restore-face-snapshot source-face source-snapshot)
      (packlet-test--restore-face-snapshot target-face target-snapshot)
      (when (file-exists-p source-file)
        (delete-file source-file))
      (delete-directory directory t))))

(ert-deftest packlet-test-advice-keyword ()
  (let ((source-file (make-temp-file "packlet-test-advice-" nil ".el"))
        (result nil))
    (unwind-protect
        (progn
          (fset 'packlet-test-advised-target
                (lambda ()
                  (setq result (append result '(target)))
                  'done))
          (fset 'packlet-test-before-advice
                (lambda (&rest _args)
                  (setq result (append result '(before)))))
          (with-temp-buffer
            (emacs-lisp-mode)
            (setq buffer-file-name source-file)
            (insert "(packlet packlet-test-advice-feature\n\
  :advice ((packlet-test-advised-target :before packlet-test-before-advice\n\
            :depth -10)))\n")
            (goto-char (point-min))
            (eval-buffer))
          (should (eq (funcall 'packlet-test-advised-target) 'done))
          (should (equal result '(before target)))
          (setq result nil)
          (with-temp-buffer
            (emacs-lisp-mode)
            (setq buffer-file-name source-file)
            (insert ";; removed\n")
            (goto-char (point-min))
            (eval-buffer))
          (should (eq (funcall 'packlet-test-advised-target) 'done))
          (should (equal result '(target))))
      (packlet-test--cleanup-symbols
       '(packlet-test-advised-target packlet-test-before-advice))
      (when (file-exists-p source-file)
        (delete-file source-file)))))

(provide 'packlet-test-keywords)

;;; packlet-test-keywords.el ends here
