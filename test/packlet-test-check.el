;;; packlet-test-check.el --- Non-evaluating checker tests -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Diagnostics must not execute any checked configuration code.

;;; Code:

(eval-and-compile
  (require 'packlet-test-support))

(defvar packlet-test-check-ran nil)

(defun packlet-test-check-string (text)
  "Return diagnostics for configuration TEXT."
  (with-temp-buffer
    (insert text)
    (packlet-check)))

(defun packlet-test-check-codes (text)
  "Return diagnostic codes for configuration TEXT."
  (mapcar (lambda (entry) (plist-get entry :code)) (packlet-test-check-string text)))

(ert-deftest packlet-test-check-never-evaluates-user-code ()
  (let ((packlet-test-check-ran nil)
        (packlet--user-keywords
         (list (cons :unsafe
                     (list :normalize (lambda (_forms) (setq packlet-test-check-ran t))
                           :expand (lambda (_context _forms) (setq packlet-test-check-ran t))))))
        (before (hash-table-count packlet--site-features)))
    (should-not
     (packlet-test-check-string
      "(setq packlet-test-check-ran t)
(packlet emacs :id safe
  :when (setq packlet-test-check-ran t)
  :setq (packlet-test-check-ran (error \"must not evaluate\"))
  :unsafe (error \"must not normalize or expand\")
  :config (setq packlet-test-check-ran t)
  :cleanup (setq packlet-test-check-ran t))"))
    (should-not packlet-test-check-ran)
    (should (= before (hash-table-count packlet--site-features)))))

(ert-deftest packlet-test-check-unknown-keywords-have-suggestions ()
  (let ((diagnostics (packlet-test-check-string "\n(packlet emacs :commmands ignore)")))
    (should (= (length diagnostics) 1))
    (should (eq (plist-get (car diagnostics) :code) :unknown-keyword))
    (should (= (plist-get (car diagnostics) :line) 2))
    (should (string-match-p ":commands" (plist-get (car diagnostics) :message))))
  (should-error (macroexpand '(packlet emacs :commmands ignore))))

(ert-deftest packlet-test-check-invalid-options-and-reader-errors ()
  (dolist (text '("(packlet emacs :hook (some-hook ignore :local maybe))"
                  "(packlet emacs :id nil)"
                  "(packlet emacs :idle -1)"
                  "(packlet emacs :after (:or))"
                  "(packlet emacs :when t nil)"
                  "(packlet emacs :cleanup (ignore))"))
    (should (memq :invalid-declaration (packlet-test-check-codes text))))
  (should (memq :read-error (packlet-test-check-codes "(packlet emacs :init")))
  (should-not (packlet-test-check-string ";; comment\n\n")))

(ert-deftest packlet-test-check-duplicates-and-nested-declarations ()
  (let ((codes (packlet-test-check-codes
                "(progn
  (packlet emacs :id same :bind (\"C-c a\" . ignore))
  (packlet emacs :id same :bind-after-load
    (emacs (:map global-map ([3 97] . ignore)))))")))
    (should (memq :duplicate-id codes))
    (should (memq :duplicate-binding codes))))

(ert-deftest packlet-test-check-skips-quoted-data-and-symbol-references ()
  (should-not
   (packlet-test-check-string
    "'(packlet emacs :typo t)
#'(lambda () '(packlet emacs :typo t))
`(packlet emacs :typo t)
(list packlet emacs)
(progn (packlet emacs :commands ignore))")))

(ert-deftest packlet-test-check-demand-cycles-respect-or-alternatives ()
  (let ((cyclic "(packlet packlet-test-check-a :after packlet-test-check-b :demand)
(packlet packlet-test-check-b :after packlet-test-check-a :demand)")
        (escaped "(packlet packlet-test-check-a
  :after (:or packlet-test-check-b packlet-test-external) :demand)
(packlet packlet-test-check-b :after packlet-test-check-a :demand)"))
    (should (= (cl-count :demand-cycle (packlet-test-check-codes cyclic)) 2))
    (should-not (memq :demand-cycle (packlet-test-check-codes escaped)))
    (should-not
     (memq :demand-cycle
           (packlet-test-check-codes
            "(packlet packlet-test-check-a :after packlet-test-check-a :demand nil)")))
    (should-not
     (memq :demand-cycle
           (packlet-test-check-codes
            "(packlet packlet-test-check-a :after packlet-test-check-b :demand)
(packlet packlet-test-check-b :after packlet-test-check-a :demand)
(packlet packlet-test-check-a :demand)")))))

(ert-deftest packlet-test-check-finds-libraries-without-loading-them ()
  (let* ((directory (make-temp-file "packlet-test-check-library-" t))
         (load-path (cons directory load-path))
         (file (expand-file-name "config.el" directory))
         (packlet-test-check-ran nil))
    (unwind-protect
        (progn
          (packlet-test--write-feature directory 'packlet-test-check-library
                                       "(setq packlet-test-check-ran t)")
          (packlet-test--write-file
           file "(packlet packlet-test-check-library
  :after packlet-test-check-missing :demand t)")
          (let ((codes (mapcar (lambda (entry) (plist-get entry :code))
                               (packlet-check file))))
            (should-not (memq :missing-library codes))
            (should (memq :missing-dependency-library codes)))
          (should-not packlet-test-check-ran)
          (should-not (featurep 'packlet-test-check-library))
          (should (memq :missing-library
                        (packlet-test-check-codes "(packlet packlet-test-check-missing)"))))
      (delete-directory directory t))))

(provide 'packlet-test-check)

;;; packlet-test-check.el ends here
