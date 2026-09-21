;;; packlet-test-after.el --- Dependency expression tests -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; AND/OR dependency gates, watchers, and compiled declarations.

;;; Code:

(eval-and-compile
  (require 'packlet-test-support))

(defvar packlet-test-after-count 0)
(defvar packlet-test-after-cleanups 0)
(defvar packlet-test-after-map)

(ert-deftest packlet-test-after-expressions-normalize-and-validate ()
  (should (equal (packlet--normalize-afters '(a (b c) nil)) '(a b c)))
  (should (equal (packlet--normalize-afters '((:or a (:and b c)) d))
                 '((:or a (:and b c)) d)))
  (should (equal (packlet--normalize-afters '((:and a b))) '(a b)))
  (dolist (form '((:or) (:and) (:not a) (:or nil a) (:or a 42) (a . b)))
    (should-error (packlet--normalize-afters (list form)))))

(ert-deftest packlet-test-after-expressions-report-only-unsatisfied-branches ()
  (cl-progv '(features) '((a b))
    (should (packlet--all-features-loaded-p '(a (:or b c))))
    (should-not (packlet--all-features-loaded-p '(a (:and b c))))
    (should-not (packlet--missing-afters '(a (:or b c))))
    (should (equal (packlet--missing-afters '((:or c d) (:and a e))) '(c d e)))
    (should (equal (packlet--after-features '(a (:or a (:and b c)))) '(a b c)))
    (should (equal (packlet--config-status
                    'a '(:has-config t :afters ((:or b c))))
                   "ready"))))

(ert-deftest packlet-test-demand-and-config-wait-for-and-or-dependencies ()
  (let* ((directory (make-temp-file "packlet-test-after-expr-" t))
         (load-path (cons directory load-path))
         (packlet-test-after-count 0)
         (names '(packlet-test-after-main packlet-test-after-a
                  packlet-test-after-b packlet-test-after-c)))
    (unwind-protect
        (progn
          (dolist (name names) (packlet-test--write-feature directory name ""))
          (with-temp-buffer
            (emacs-lisp-mode)
            (eval '(packlet packlet-test-after-main :id expressions
                     :after (:and packlet-test-after-a
                                  (:or packlet-test-after-b packlet-test-after-c))
                     :config (cl-incf packlet-test-after-count)
                     :demand t))
            (require 'packlet-test-after-a)
            (should-not (featurep 'packlet-test-after-main))
            (require 'packlet-test-after-c)
            (should (featurep 'packlet-test-after-main))
            (should (= packlet-test-after-count 1))
            (require 'packlet-test-after-b)
            (should (= packlet-test-after-count 1))
            (should-not (gethash :or packlet--after-load-handlers))
            (should-not (gethash :and packlet--after-load-handlers))))
      (dolist (name names) (packlet-test--cleanup-feature name))
      (delete-directory directory t))))

(ert-deftest packlet-test-idle-or-dependencies-schedule-on-either-feature ()
  (cl-progv '(features after-init-time) (list (copy-sequence features) t)
    (let ((loaded nil))
      (packlet-test-with-idle-timers scheduled
        (with-temp-buffer
          (emacs-lisp-mode)
          (eval '(packlet packlet-test-idle-or-main :id idle-or
                   :after (:or packlet-test-idle-or-a packlet-test-idle-or-b)
                   :idle 0.5))
          (should-not scheduled)
          (provide 'packlet-test-idle-or-b)
          (packlet--run-after-load-handlers 'packlet-test-idle-or-b)
          (should (= (length scheduled) 1))
          (provide 'packlet-test-idle-or-a)
          (packlet--run-after-load-handlers 'packlet-test-idle-or-a)
          (should (= (length scheduled) 1))
          (cl-letf (((symbol-function 'packlet--load-feature)
                     (lambda (feature &optional _file) (setq loaded feature)))
                    ((symbol-function 'packlet--idle-load-ready-p) (lambda () t)))
            (packlet-test--invoke-scheduled-timer (car scheduled)))
          (should (eq loaded 'packlet-test-idle-or-main)))))))

(ert-deftest packlet-test-keymap-watchers-follow-or-leaves ()
  (cl-progv '(features) (list (copy-sequence features))
    (unwind-protect
        (with-temp-buffer
          (emacs-lisp-mode)
          (makunbound 'packlet-test-after-map)
          (eval '(packlet packlet-test-map-or-main :id map-or
                   :after (:or packlet-test-map-or-a packlet-test-map-or-b)
                   :bind (:map packlet-test-after-map ("a" . ignore))))
          (setq packlet-test-after-map (make-sparse-keymap))
          (provide 'packlet-test-map-or-b)
          (packlet--run-after-load-handlers 'packlet-test-map-or-b)
          (should (eq (lookup-key packlet-test-after-map "a") 'ignore))
          (packlet-cleanup-declaration 'map-or)
          (should-not (lookup-key packlet-test-after-map "a")))
      (makunbound 'packlet-test-after-map))))

(ert-deftest packlet-test-compiled-named-or-config-and-cleanup ()
  (let* ((directory (make-temp-file "packlet-test-compiled-or-" t))
         (file (expand-file-name "config.el" directory))
         (packlet-test-after-count 0)
         (packlet-test-after-cleanups 0)
         (packlet--after-load-handlers (make-hash-table :test #'eq))
         (packlet--after-load-dispatchers (make-hash-table :test #'eq))
         (after-load-alist nil))
    (unwind-protect
        (progn
          (packlet-test--write-file
           file
           ";;; -*- lexical-binding: t; -*-
(eval-when-compile (require 'packlet))
(require 'packlet-runtime)
(defvar packlet-test-after-count 0)
(defvar packlet-test-after-cleanups 0)
(packlet emacs :id compiled-or
  :after (:or packlet-test-never emacs)
  :config (cl-incf packlet-test-after-count)
  :cleanup (cl-incf packlet-test-after-cleanups))")
          (dolist (tracking '(t nil))
            (let ((packlet-expand-source-tracking tracking))
              (should (byte-compile-file file)))
            (load (concat file "c") nil t)
            (should (= packlet-test-after-count (if tracking 1 2)))
            (packlet-cleanup-source file)
            (should (= packlet-test-after-cleanups 1))))
      (packlet-cleanup-source file)
      (delete-directory directory t))))

(provide 'packlet-test-after)

;;; packlet-test-after.el ends here
