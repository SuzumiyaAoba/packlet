;;; packlet-test-source.el --- Source tracking tests for packlet -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Tests for source tracking, reevaluation, and inspection helpers.

;;; Code:

(eval-and-compile
  (require 'packlet-test-support))

(defvar packlet-test-hook-count)
(defvar packlet-test-explain-config-ran)

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

(ert-deftest packlet-test-source-session-merges-failed-and-new-entry-with-same-id ()
  (let ((scope '(:buffer merge-same-buffer))
        (cleanup-calls nil)
        (warnings nil))
    (cl-letf (((symbol-function 'display-warning)
               (lambda (_type message &optional _level)
                 (push message warnings))))
      (packlet--set-source-entries
       scope
       (list
        (packlet--make-source-entry
         :id 'same
         :install #'ignore
         :cleanup (lambda ()
                    (push :old cleanup-calls)
                    (error "boom")))))
      (packlet--with-source-session
       scope
       nil
       (lambda ()
         (packlet--register-source-entry
          scope
          'same
          #'ignore
          (lambda ()
            (push :new cleanup-calls)))))
      (should (= (length warnings) 1))
      (should (equal (mapcar #'packlet--source-entry-id
                             (packlet--source-entries scope))
                     '(same)))
      (setq cleanup-calls nil)
      (let ((failed
             (packlet--run-source-entry-cleanups
              (packlet--source-entries scope)
              scope)))
        (should (equal cleanup-calls '(:old :new)))
        (should (= (length failed) 1))))))

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

(ert-deftest packlet-test-describe-source-file-scope ()
  (let ((source-file (make-temp-file "packlet-test-describe-" nil ".el")))
    (defvar packlet-test-describe-hook nil)
    (unwind-protect
        (progn
          (with-temp-buffer
            (emacs-lisp-mode)
            (setq buffer-file-name source-file)
            (insert "(packlet packlet-test-describe-feature\n\
  :setq (packlet-test-tracked-setq 7)\n\
  :hook ((packlet-test-describe-hook . ignore)))\n")
            (goto-char (point-min))
            (eval-buffer))
          (let ((description (packlet-describe-source source-file)))
            (should (string-match-p (regexp-quote source-file) description))
            (should (string-match-p ":setq" description))
            (should (string-match-p ":hook" description))))
      (ignore-errors
        (with-temp-buffer
          (emacs-lisp-mode)
          (setq buffer-file-name source-file)
          (insert ";; removed\n")
          (goto-char (point-min))
          (eval-buffer)))
      (packlet-test--cleanup-symbols '(packlet-test-describe-hook))
      (when (file-exists-p source-file)
        (delete-file source-file)))))

(ert-deftest packlet-test-describe-source-shows-hook-setq ()
  (let ((source-file (make-temp-file "packlet-test-describe-hook-setq-" nil ".el")))
    (defvar packlet-test-describe-hook-setq nil)
    (unwind-protect
        (progn
          (with-temp-buffer
            (emacs-lisp-mode)
            (setq buffer-file-name source-file)
            (insert "(packlet packlet-test-describe-hook-setq-feature\n\
  :hook-setq ((packlet-test-describe-hook-setq\n\
               (packlet-test-hook-setq-value 42))))\n")
            (goto-char (point-min))
            (eval-buffer))
          (let ((description (packlet-describe-source source-file)))
            (should (string-match-p (regexp-quote source-file) description))
            (should (string-match-p ":hook-setq" description))))
      (ignore-errors
        (with-temp-buffer
          (emacs-lisp-mode)
          (setq buffer-file-name source-file)
          (insert ";; removed\n")
          (goto-char (point-min))
          (eval-buffer)))
      (packlet-test--cleanup-symbols '(packlet-test-describe-hook-setq))
      (when (file-exists-p source-file)
        (delete-file source-file)))))

(ert-deftest packlet-test-describe-source-shows-hook-add-and-hook-enable ()
  (let ((source-file (make-temp-file "packlet-test-describe-hook-extra-" nil ".el")))
    (defvar packlet-test-describe-hook-extra nil)
    (defvar packlet-test-describe-hook-target nil)
    (unwind-protect
        (progn
          (with-temp-buffer
            (emacs-lisp-mode)
            (setq buffer-file-name source-file)
            (insert "(packlet packlet-test-describe-hook-extra-feature\n\
  :hook-add ((packlet-test-describe-hook-extra packlet-test-describe-hook-target ignore))\n\
  :hook-enable ((packlet-test-describe-hook-extra packlet-test-hook-enable-mode)))\n")
            (goto-char (point-min))
            (eval-buffer))
          (let ((description (packlet-describe-source source-file)))
            (should (string-match-p ":hook-add" description))
            (should (string-match-p ":hook-enable" description))))
      (ignore-errors
        (with-temp-buffer
          (emacs-lisp-mode)
          (setq buffer-file-name source-file)
          (insert ";; removed\n")
          (goto-char (point-min))
          (eval-buffer)))
      (packlet-test--cleanup-symbols
       '(packlet-test-describe-hook-extra packlet-test-describe-hook-target))
      (when (file-exists-p source-file)
        (delete-file source-file)))))

(ert-deftest packlet-test-describe-feature-groups-source-entries ()
  (let ((source-file (make-temp-file "packlet-test-describe-feature-" nil ".el")))
    (defvar packlet-test-describe-feature-hook nil)
    (unwind-protect
        (progn
          (with-temp-buffer
            (emacs-lisp-mode)
            (setq buffer-file-name source-file)
            (insert "(packlet packlet-test-describe-feature-target\n\
  :setq (packlet-test-tracked-setq 7)\n\
  :hook ((packlet-test-describe-feature-hook . ignore)))\n")
            (goto-char (point-min))
            (eval-buffer))
          (let ((description
                 (packlet-describe-feature 'packlet-test-describe-feature-target)))
            (should (string-match-p "Feature: packlet-test-describe-feature-target"
                                    description))
            (should (string-match-p (regexp-quote source-file) description))
            (should (string-match-p ":setq" description))
            (should (string-match-p ":hook" description))))
      (ignore-errors
        (with-temp-buffer
          (emacs-lisp-mode)
          (setq buffer-file-name source-file)
          (insert ";; removed\n")
          (goto-char (point-min))
          (eval-buffer)))
      (packlet-test--cleanup-symbols '(packlet-test-describe-feature-hook))
      (when (file-exists-p source-file)
        (delete-file source-file)))))

(ert-deftest packlet-test-list-features-shows-registered-features ()
  (let ((source-file (make-temp-file "packlet-test-list-features-" nil ".el"))
        (packlet--site-features (make-hash-table :test #'equal))
        (packlet--file-source-states (make-hash-table :test #'equal))
        (packlet--buffer-source-states (make-hash-table :test #'eq :weakness 'key))
        (packlet--after-load-handlers (make-hash-table :test #'eq))
        (packlet--after-load-dispatchers (make-hash-table :test #'eq))
        (packlet--idle-load-states (make-hash-table :test #'equal))
        (packlet--expansion-load-states (make-hash-table :test #'equal))
        (after-load-alist nil))
    (unwind-protect
        (progn
          (with-temp-buffer
            (emacs-lisp-mode)
            (setq buffer-file-name source-file)
            (insert "(packlet packlet-test-list-feature-a\n\
  :setq (packlet-test-tracked-setq 1))\n\
\n\
(packlet packlet-test-list-feature-b\n\
  :config nil)\n")
            (goto-char (point-min))
            (eval-buffer))
          (let ((description (packlet-list-features)))
            (should (string-match-p "Features: 2" description))
            (should (string-match-p "packlet-test-list-feature-a" description))
            (should (string-match-p "packlet-test-list-feature-b" description))
            (should (string-match-p "sources=1" description))))
      (ignore-errors
        (with-temp-buffer
          (emacs-lisp-mode)
          (setq buffer-file-name source-file)
          (insert ";; removed\n")
          (goto-char (point-min))
          (eval-buffer)))
      (when (file-exists-p source-file)
        (delete-file source-file)))))

(ert-deftest packlet-test-explain-feature-shows-runtime-state ()
  (let* ((directory (make-temp-file "packlet-test-" t))
         (load-path (cons directory load-path))
         (after-init-time nil)
         (emacs-startup-hook nil)
         (feature 'packlet-test-explain-feature)
         (after-feature 'packlet-test-explain-after)
         (source-file (expand-file-name "packlet-test-explain-config.el"
                                        directory)))
    (unwind-protect
        (progn
          (setq packlet-test-explain-config-ran nil)
          (packlet-test--cleanup-feature feature)
          (packlet-test--cleanup-feature after-feature)
          (packlet-test--write-feature
           directory
           after-feature
           "(defvar packlet-test-explain-after-loaded t)")
          (packlet-test--write-feature
           directory
           feature
           "(defvar packlet-test-explain-feature-loaded t)")
          (with-temp-buffer
            (emacs-lisp-mode)
            (setq buffer-file-name source-file)
            (insert "(packlet packlet-test-explain-feature\n\
  :after packlet-test-explain-after\n\
  :idle\n\
  :demand t\n\
  :config\n\
  (setq packlet-test-explain-config-ran t))\n")
            (goto-char (point-min))
            (eval-buffer))
          (let ((before (packlet-explain-feature feature)))
            (should (string-match-p "Loaded: no" before))
            (should (string-match-p "Afters: packlet-test-explain-after" before))
            (should (string-match-p "Missing afters: packlet-test-explain-after"
                                    before))
            (should (string-match-p "Config: waiting for feature" before))
            (should (string-match-p "Demand: waiting for afters" before))
            (should (string-match-p "Idle: waiting for afters" before)))
          (require after-feature)
          (let ((after (packlet-explain-feature feature)))
            (should (featurep feature))
            (should (bound-and-true-p packlet-test-explain-config-ran))
            (should (string-match-p "Loaded: yes" after))
            (should (string-match-p "Missing afters: none" after))
            (should (string-match-p "Config: done" after))
            (should (string-match-p "Demand: loaded" after))
            (should (string-match-p "Idle: loaded" after))))
      (ignore-errors
        (with-temp-buffer
          (emacs-lisp-mode)
          (setq buffer-file-name source-file)
          (insert ";; removed\n")
          (goto-char (point-min))
          (eval-buffer)))
      (setq packlet-test-explain-config-ran nil)
      (packlet-test--cleanup-feature feature)
      (packlet-test--cleanup-feature after-feature)
      (when (file-exists-p source-file)
        (delete-file source-file))
      (delete-directory directory t))))

(ert-deftest packlet-test-cleanup-source-file-scope ()
  (let ((source-file (make-temp-file "packlet-test-cleanup-" nil ".el")))
    (defvar packlet-test-cleanup-hook nil)
    (unwind-protect
        (progn
          (setq packlet-test-tracked-setq 10
                packlet-test-cleanup-hook nil
                packlet-test-hook-count 0)
          (with-temp-buffer
            (emacs-lisp-mode)
            (setq buffer-file-name source-file)
            (insert "(packlet packlet-test-cleanup-feature\n\
  :setq (packlet-test-tracked-setq 7)\n\
  :hook ((packlet-test-cleanup-hook . (lambda ()\n\
                                        (setq packlet-test-hook-count\n\
                                              (1+ packlet-test-hook-count))))))\n")
            (goto-char (point-min))
            (eval-buffer))
          (should (= packlet-test-tracked-setq 7))
          (run-hooks 'packlet-test-cleanup-hook)
          (should (= packlet-test-hook-count 1))
          (should-not (packlet-cleanup-source source-file))
          (should (= packlet-test-tracked-setq 10))
          (setq packlet-test-hook-count 0)
          (run-hooks 'packlet-test-cleanup-hook)
          (should (= packlet-test-hook-count 0))
          (should-not (packlet--source-entries
                       (packlet--source-scope-file source-file))))
      (setq packlet-test-hook-count nil)
      (setq packlet-test-tracked-setq 0)
      (packlet-test--cleanup-symbols '(packlet-test-cleanup-hook))
      (when (file-exists-p source-file)
        (delete-file source-file)))))

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

(ert-deftest packlet-test-deep-nonfile-eval-detects-packlet-in-non-lisp-buffer ()
  (defvar packlet-test-deep-nonlisp-hook nil)
  (unwind-protect
      (with-temp-buffer
        (fundamental-mode)
        (setq packlet-test-hook-count 0
              packlet-test-deep-nonlisp-hook nil)
        (eval
         (packlet-test--nest-progns
          5
          '(packlet packlet-test-deep-nonlisp-feature
             :hook ((packlet-test-deep-nonlisp-hook
                     . (lambda ()
                         (setq packlet-test-hook-count
                               (1+ packlet-test-hook-count))))))))
        (eval
         (packlet-test--nest-progns
          5
          '(packlet packlet-test-deep-nonlisp-feature
             :hook ((packlet-test-deep-nonlisp-hook
                     . (lambda ()
                         (setq packlet-test-hook-count
                               (+ packlet-test-hook-count 10))))))))
        (run-hooks 'packlet-test-deep-nonlisp-hook)
        (should (= packlet-test-hook-count 10)))
    (setq packlet-test-hook-count nil)
    (packlet-test--cleanup-symbols '(packlet-test-deep-nonlisp-hook))))

(provide 'packlet-test-source)

;;; packlet-test-source.el ends here
