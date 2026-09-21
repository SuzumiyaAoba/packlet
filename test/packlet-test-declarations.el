;;; packlet-test-declarations.el --- Named declarations and cleanup -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Declaration-level transactions and explicit configuration cleanup.

;;; Code:

(eval-and-compile
  (require 'packlet-test-support))

(defvar packlet-test-declaration-a 0)
(defvar packlet-test-declaration-b 0)
(defvar packlet-test-declaration-events nil)
(defvar packlet-test-declaration-fail nil)

(ert-deftest packlet-test-id-validates-literal-identifiers ()
  (dolist (id '(first "first"))
    (should (equal (packlet--id-form (list (list :id id))) id)))
  (dolist (forms '(nil (nil) (t) (42) ("") ((quote first)) (first second)))
    (should-error (packlet--id-form (list (cons :id forms))))))

(ert-deftest packlet-test-named-eval-replaces-only-one-declaration ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (let ((packlet-test-declaration-a 0)
          (packlet-test-declaration-b 0))
      (eval '(packlet emacs :id first :setq (packlet-test-declaration-a 1)))
      (eval '(packlet emacs :id second :setq (packlet-test-declaration-b 2)))
      (eval '(packlet emacs :id first :setq (packlet-test-declaration-a 3)))
      (should (= packlet-test-declaration-a 3))
      (should (= packlet-test-declaration-b 2))
      (packlet-cleanup-declaration 'first)
      (should (= packlet-test-declaration-a 0))
      (should (= packlet-test-declaration-b 2))
      (packlet-cleanup-declaration 'second)
      (should (= packlet-test-declaration-b 0)))))

(ert-deftest packlet-test-named-buffer-sites-survive-renames-and-stay-distinct ()
  (let ((first (generate-new-buffer " *packlet-named-first*"))
        (second (generate-new-buffer " *packlet-named-second*"))
        (packlet-test-declaration-a 0)
        (packlet-test-declaration-b 0))
    (unwind-protect
        (progn
          (with-current-buffer first
            (emacs-lisp-mode)
            (eval '(packlet emacs :id same :setq (packlet-test-declaration-a 1)))
            (rename-buffer " *packlet-named-renamed*" t)
            (eval '(packlet emacs :id same :setq (packlet-test-declaration-a 2))))
          (with-current-buffer second
            (emacs-lisp-mode)
            (eval '(packlet emacs :id same :setq (packlet-test-declaration-b 3))))
          (packlet-cleanup-declaration 'same first)
          (should (= packlet-test-declaration-a 0))
          (should (= packlet-test-declaration-b 3))
          (packlet-cleanup-declaration 'same second)
          (should (= packlet-test-declaration-b 0)))
      (kill-buffer first)
      (kill-buffer second))))

(ert-deftest packlet-test-named-file-eval-and-rollback-preserve-other-sites ()
  (let ((source (make-temp-file "packlet-test-named-" nil ".el"))
        (packlet-test-declaration-a 0)
        (packlet-test-declaration-b 0))
    (unwind-protect
        (with-temp-buffer
          (emacs-lisp-mode)
          (setq buffer-file-name source)
          (insert ";;; -*- lexical-binding: t; -*-\n"
                  "(packlet emacs :id first :setq (packlet-test-declaration-a 1))\n"
                  "(packlet emacs :id second :setq (packlet-test-declaration-b 2))\n")
          (eval-buffer)
          (packlet-eval-declaration
           '(packlet emacs :id first :setq (packlet-test-declaration-a 3)))
          (should-error
           (packlet-eval-declaration
            '(packlet emacs :id first
               :setq (packlet-test-declaration-a 4)
               :config (error "failed configuration"))))
          (should (= packlet-test-declaration-a 3))
          (should (= packlet-test-declaration-b 2))
          (should (string-match-p "Declaration: first" (packlet-explain-feature 'emacs)))
          (packlet-cleanup-declaration 'first source)
          (should (= packlet-test-declaration-a 0))
          (should (= packlet-test-declaration-b 2)))
      (packlet-cleanup-source source)
      (delete-file source))))

(ert-deftest packlet-test-eval-declaration-at-point ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (let ((packlet-test-declaration-a 0))
      (insert "(packlet emacs :id at-point\n  :setq (packlet-test-declaration-a 7))")
      (goto-char (point-min))
      (packlet-eval-declaration)
      (should (= packlet-test-declaration-a 7))
      (goto-char (point-max))
      (packlet-eval-declaration)
      (packlet-cleanup-declaration 'at-point)
      (should (= packlet-test-declaration-a 0))
      (should-error (packlet-eval-declaration '(packlet emacs :init nil)))
      (should-error (packlet-eval-declaration '(progn nil))))))

(ert-deftest packlet-test-named-sites-stay-stable-after-reordering ()
  (let ((source (make-temp-file "packlet-test-named-move-" nil ".el")))
    (unwind-protect
        (progn
          (packlet-test-eval-in-file
           source "(packlet emacs :id one :config nil)\n(packlet emacs :id two :config nil)")
          (let* ((scope (packlet--source-scope-file source))
                 (site (packlet--named-site scope 'one))
                 (before (plist-get (gethash site packlet--site-features) :configured-var)))
            (packlet-test-eval-in-file
             source "(packlet emacs :id two :config nil)\n\n(packlet emacs :id one :config nil)")
            (should (eq before (plist-get (gethash site packlet--site-features) :configured-var)))
            (should (= (length (cl-remove-if-not
                                (lambda (entry) (eq (packlet--source-entry-kind entry) :site-feature))
                                (packlet--source-entries scope)))
                       2))))
      (packlet-cleanup-source source)
      (delete-file source))))

(ert-deftest packlet-test-duplicate-ids-roll-back-file ()
  (let ((source (make-temp-file "packlet-test-duplicate-id-" nil ".el"))
        (packlet-test-declaration-a 0))
    (unwind-protect
        (progn
          (packlet-test-eval-in-file source
                                    "(packlet emacs :id one :setq (packlet-test-declaration-a 1))")
          (should-error
           (packlet-test-eval-in-file
            source "(packlet emacs :id one :setq (packlet-test-declaration-a 2))
                    (packlet cl-lib :id one :init nil)"))
          (should (= packlet-test-declaration-a 1)))
      (packlet-cleanup-source source)
      (delete-file source))))

(ert-deftest packlet-test-cleanup-runs-on-reevaluation-and-removal ()
  (let ((packlet-test-declaration-events nil))
    (with-temp-buffer
      (emacs-lisp-mode)
      (let ((form '(packlet emacs :id lifecycle
                     :config (push 'start packlet-test-declaration-events)
                     :cleanup (push 'stop packlet-test-declaration-events))))
        (eval form)
        (eval form)
        (should (equal packlet-test-declaration-events '(start stop start)))
        (packlet-cleanup-declaration 'lifecycle)
        (packlet-cleanup-declaration 'lifecycle)
        (should (equal packlet-test-declaration-events '(stop start stop start)))))))

(ert-deftest packlet-test-cleanup-runs-after-partial-config-failure ()
  (let ((packlet-test-declaration-events nil))
    (with-temp-buffer
      (emacs-lisp-mode)
      (should-error
       (eval '(packlet emacs :id failed
                :config
                (push 'start packlet-test-declaration-events)
                (error "partially configured")
                :cleanup (push 'stop packlet-test-declaration-events))))
      (should-not (packlet--source-entries (packlet--buffer-source-scope))))
    (should (equal packlet-test-declaration-events '(stop start)))))

(ert-deftest packlet-test-cleanup-skips-pending-and-guarded-configs ()
  (let ((packlet-test-declaration-events nil))
    (with-temp-buffer
      (emacs-lisp-mode)
      (eval '(packlet emacs :id pending
               :after packlet-test-not-yet-loaded
               :config (push 'start packlet-test-declaration-events)
               :cleanup (push 'stop packlet-test-declaration-events)))
      (eval '(packlet emacs :id guarded :when nil
               :config (push 'start packlet-test-declaration-events)
               :cleanup (push 'stop packlet-test-declaration-events)))
      (packlet-cleanup-source)
      (should-not packlet-test-declaration-events))))

(ert-deftest packlet-test-cleanup-failure-can-be-retried ()
  (let ((packlet-test-declaration-fail t)
        (packlet-test-declaration-events nil)
        (packlet-warn-on-missing-libraries nil))
    (with-temp-buffer
      (emacs-lisp-mode)
      (eval '(packlet emacs :id retry
               :config nil
               :cleanup
               (when packlet-test-declaration-fail (error "cleanup failed"))
               (push 'stop packlet-test-declaration-events)))
      (should (= (length (packlet-cleanup-declaration 'retry)) 1))
      (setq packlet-test-declaration-fail nil)
      (should-not (packlet-cleanup-declaration 'retry))
      (should-not (packlet--source-entries (packlet--buffer-source-scope)))
      (should (equal packlet-test-declaration-events '(stop))))))

(ert-deftest packlet-test-cleanup-requires-config ()
  (should-error (macroexpand '(packlet emacs :cleanup (ignore))))
  (should-error (macroexpand '(packlet emacs :config :cleanup (ignore)))))

(provide 'packlet-test-declarations)

;;; packlet-test-declarations.el ends here
