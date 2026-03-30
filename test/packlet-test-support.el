;;; packlet-test-support.el --- Shared test support for packlet -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Shared helpers and state for `packlet' ERT tests.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'packlet)

(defvar packlet-test-command-ran)
(defvar packlet-test-hook-ran)
(defvar packlet-test-helper-ran)
(defvar packlet-test-interpreter-ran)
(defvar packlet-test-magic-ran)
(defvar packlet-test-magic-fallback-ran)
(defvar packlet-test-prefix-command-ran)
(defvar packlet-test-prefix-map-command-ran)
(defvar packlet-test-parent-map)
(defvar packlet-test-child-map)
(defvar packlet-test-custom-value)
(defvar packlet-test-compile-variable)
(defvar packlet-test-compile-function-ran)
(defvar packlet-test-compile-config-ran)
(defvar packlet-test-compile-second-config-ran)
(defvar packlet-test-explain-config-ran)
(defvar packlet-test-reeval-result)
(defvar packlet-test-hook-count)
(defvar packlet-test-hook-setq-value nil)
(defvar packlet-test-hook-setq-other nil)
(defvar packlet-test-hook-add-count)
(defvar packlet-test-hook-enable-mode nil)
(defvar packlet-test-list-a nil)
(defvar packlet-test-list-b nil)
(defvar packlet-test-compare-list nil)
(defvar packlet-test-alist nil)
(defvar packlet-test-managed-list nil)
(defvar packlet-test-prefix-keyword-map nil)
(defvar packlet-test-prefix-map-remove-map nil)
(defvar packlet-test-bind-after-load-map nil)
(defvar packlet-test-enable-mode nil)
(defvar packlet-test-tracked-setq 0)
(defvar packlet-test-hook-when-enabled nil)
(defcustom packlet-test-tracked-custom 0
  "User option used by `packlet' setting tests."
  :type 'integer
  :group 'packlet)

(declare-function packlet-test-hook "packlet-test-feature" ())
(declare-function packlet-test-startup-hook "packlet-test-multi-hook-feature" ())
(declare-function packlet-test-major-mode-hook "packlet-test-multi-hook-feature" ())
(declare-function packlet-test-al-custom-file "packlet-test-al-lib" ())
(declare-function packlet-test-advised-target nil ())
(declare-function packlet-test-enable-mode nil (&optional arg))
(declare-function packlet-test-derived-mode nil ())

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

(defun packlet-test--restore-face-snapshot (face snapshot)
  "Restore FACE from SNAPSHOT captured by `packlet--face-snapshot'."
  (unless (facep face)
    (make-empty-face face))
  (apply
   #'set-face-attribute face nil
   (cl-loop for (attribute . value) in snapshot
            append (list attribute value))))

(defun packlet-test--invoke-scheduled-timer (entry)
  "Invoke a fake timer ENTRY captured by test stubs."
  (apply (plist-get entry :fn) (plist-get entry :args)))

(defun packlet-test--nest-progns (depth form)
  "Wrap FORM in DEPTH nested `progn' forms."
  (dotimes (_ depth form)
    (setq form `(progn ,form))))

(defun packlet-test-eval-in-file (source-file code)
  "Evaluate CODE string in a temp buffer visiting SOURCE-FILE."
  (with-temp-buffer
    (emacs-lisp-mode)
    (setq buffer-file-name source-file)
    (insert code)
    (goto-char (point-min))
    (eval-buffer)))

(defmacro packlet-test-with-idle-timers (scheduled-var &rest body)
  "Execute BODY with idle-timer functions stubbed.
SCHEDULED-VAR is bound to a list that accumulates timer entries.
Each entry is a plist with :timer, :secs, :repeat, :fn, :args.
Also binds `timer-id' (counter) and stubs `timerp' and `cancel-timer'."
  (declare (indent 1) (debug (symbolp body)))
  `(let ((timer-id 0)
         (,scheduled-var nil)
         (cancelled nil))
     (cl-letf (((symbol-function 'run-with-idle-timer)
                (lambda (secs repeat fn &rest args)
                  (let ((timer (list :timer (cl-incf timer-id))))
                    (push (list :timer timer :secs secs :repeat repeat
                                :fn fn :args args)
                          ,scheduled-var)
                    timer)))
               ((symbol-function 'timerp)
                (lambda (object)
                  (and (consp object)
                       (eq (car object) :timer))))
               ((symbol-function 'cancel-timer)
                (lambda (timer)
                  (push timer cancelled))))
       ,@body)))

(provide 'packlet-test-support)

;;; packlet-test-support.el ends here
