;;; packlet-test-static.el --- Compiled startup tests -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Immutable configurations can omit reevaluation bookkeeping at build time.

;;; Code:

(eval-and-compile
  (require 'packlet-test-support))

(defvar packlet-test-static-evaluations 0)
(defvar packlet-test-static-value nil)
(defvar packlet-test-static-setter-values nil)
(defcustom packlet-test-static-option 0
  "Option used to check that static expansion retains custom setters."
  :type 'integer
  :group 'packlet
  :set (lambda (symbol value)
         (push value packlet-test-static-setter-values)
         (set-default symbol value)))

(ert-deftest packlet-test-static-settings-evaluate-once-and-call-setters ()
  (let ((packlet-expand-source-tracking nil)
        (packlet-test-static-evaluations 0)
        (packlet-test-static-value nil)
        (packlet-test-static-option 0)
        (packlet-test-static-setter-values nil)
        (packlet--site-features (make-hash-table :test #'equal)))
    (eval (macroexpand
           '(packlet packlet-test-static-settings
              :setq (packlet-test-static-value
                     (cl-incf packlet-test-static-evaluations))
              :custom (packlet-test-static-option
                       (cl-incf packlet-test-static-evaluations))))
          t)
    (should (= packlet-test-static-evaluations 2))
    (should (= packlet-test-static-value 1))
    (should (= packlet-test-static-option 2))
    (should (equal packlet-test-static-setter-values '(2)))
    (should (= (hash-table-count packlet--site-features) 0))))

(ert-deftest packlet-test-static-expansion-preserves-quoted-data ()
  (let ((packlet-expand-source-tracking nil)
        (packlet-test-static-value nil))
    (eval (macroexpand
           '(packlet packlet-test-static-data
              :init
              (setq packlet-test-static-value
                    '(packlet--register-source-entry nil id install cleanup))))
          t)
    (should (equal packlet-test-static-value
                   '(packlet--register-source-entry nil id install cleanup)))))

(ert-deftest packlet-test-autoload-discovery-can-be-disabled-and-reenabled ()
  (let* ((directory (make-temp-file "packlet-test-autoload-policy-" t))
         (load-path (cons directory load-path))
         (packlet--loaded-autoloads (make-hash-table :test #'equal))
         (function 'packlet-test-policy-command))
    (unwind-protect
        (progn
          (packlet-test--write-file
           (expand-file-name "packlet-test-policy-autoloads.el" directory)
           "(autoload 'packlet-test-policy-command \"packlet-test-policy-sub\" nil t)")
          (let ((packlet-load-package-autoloads nil))
            (packlet--maybe-autoload function "packlet-test-policy" t))
          (should (equal (packlet--current-autoload-file function)
                         "packlet-test-policy"))
          (should (= (hash-table-count packlet--loaded-autoloads) 0))
          (let ((packlet-load-package-autoloads t))
            (packlet--maybe-autoload function "packlet-test-policy" t))
          (should (equal (packlet--current-autoload-file function)
                         "packlet-test-policy-sub")))
      (packlet-test--cleanup-symbols (list function))
      (delete-directory directory t))))

(ert-deftest packlet-test-compiled-config-needs-only-runtime ()
  (let* ((directory (make-temp-file "packlet-test-runtime-" t))
         (packlet-directory (file-name-directory (locate-library "packlet")))
         (config (expand-file-name "config.el" directory))
         (probe (expand-file-name "probe.el" directory))
         (program (expand-file-name invocation-name invocation-directory)))
    (unwind-protect
        (progn
          (packlet-test--write-feature
           directory 'packlet-test-runtime-package
           "(defvar packlet-test-runtime-map (make-sparse-keymap))
(defun packlet-test-runtime-command () (interactive) 42)
(defun packlet-test-runtime-hook () (setq packlet-test-runtime-hook-ran t))")
          (packlet-test--write-feature directory 'packlet-test-runtime-dependency "")
          (packlet-test--write-feature directory 'packlet-test-runtime-idle "")
          (packlet-test--write-file
           config
           ";;; config.el --- Test -*- lexical-binding: t; -*-
(eval-when-compile (require 'packlet))
(require 'packlet-runtime)
(defvar packlet-test-runtime-config-runs 0)
(defvar packlet-test-runtime-hook-ran nil)
(defvar packlet-test-runtime-data nil)
(packlet packlet-test-runtime-package
  :init (setq packlet-test-runtime-data
              `(packlet--register-source-entry nil ,(+ 1 2) install cleanup))
  :after packlet-test-runtime-dependency
  :commands packlet-test-runtime-command
  :hook (packlet-test-runtime-hook-hook . packlet-test-runtime-hook)
  :bind (:map packlet-test-runtime-map (\"a\" . packlet-test-runtime-command))
  :config (cl-incf packlet-test-runtime-config-runs))
(packlet packlet-test-runtime-dependency :demand t)
(packlet packlet-test-runtime-idle :after packlet-test-runtime-dependency :idle 0)")
          (packlet-test--write-file
           probe
           ";;; probe.el --- Test -*- lexical-binding: t; -*-
(require 'cl-lib)
(load \"config.elc\" nil t)
(cl-assert (equal packlet-test-runtime-data
                   '(packlet--register-source-entry nil 3 install cleanup)))
(cl-assert (featurep 'packlet-test-runtime-dependency))
(cl-assert (not (featurep 'packlet-test-runtime-package)))
(cl-assert (= (packlet-test-runtime-command) 42))
(cl-assert (= packlet-test-runtime-config-runs 1))
(run-hooks 'packlet-test-runtime-hook-hook)
(cl-assert packlet-test-runtime-hook-ran)
(cl-assert (eq (lookup-key packlet-test-runtime-map \"a\") 'packlet-test-runtime-command))
(require 'packlet-test-runtime-package)
(cl-assert (= packlet-test-runtime-config-runs 1))
(maphash (lambda (id _state) (packlet--run-idle-load id)) packlet--idle-load-states)
(cl-assert (featurep 'packlet-test-runtime-idle))
(dolist (feature '(packlet packlet-source packlet-parse packlet-expand))
  (cl-assert (not (featurep feature))))")
          (dolist (tracking '(t nil))
            (let ((packlet-expand-source-tracking tracking)
                  (byte-compile-warnings nil))
              (should (byte-compile-file config)))
            (with-temp-buffer
              (let ((exit-code (call-process program nil t nil
                                             "-Q" "--batch"
                                             "-L" packlet-directory
                                             "-L" directory "-l" probe)))
                (ert-info ((buffer-string))
                  (should (equal exit-code 0)))))))
      (delete-directory directory t))))

(ert-deftest packlet-test-generated-symbol-is-stable-across-processes ()
  (let ((program (expand-file-name invocation-name invocation-directory))
        (directory (file-name-directory (locate-library "packlet")))
        results)
    (dolist (limits '(nil t))
      (with-temp-buffer
        (should
         (= 0 (call-process
               program nil t nil "-Q" "--batch" "-L" directory "--eval"
               (format
                "(progn (require 'packlet)
                   (let ((print-length %S) (print-level %S))
                     (princ (packlet--generated-symbol
                             \"packlet--test\" 'test-feature
                             '(compiled test-feature 1) \"config\"))))"
                (and limits 1) (and limits 1)))))
        (push (buffer-string) results)))
    (should (string-prefix-p "packlet--test-test-feature-config-" (car results)))
    (should (equal (car results) (cadr results)))))

(provide 'packlet-test-static)

;;; packlet-test-static.el ends here
