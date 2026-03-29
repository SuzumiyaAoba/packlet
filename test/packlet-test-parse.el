;;; packlet-test-parse.el --- Parsing tests for packlet -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Tests for parsing and user keyword support in `packlet'.

;;; Code:

(eval-and-compile
  (require 'packlet-test-support))

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

(ert-deftest packlet-test-normalize-hooks-with-options ()
  (should
   (equal
    (packlet--normalize-hooks
     '((packlet-test-hook-a . ignore)
       (packlet-test-hook-b ignore :append t)
       (packlet-test-hook-c ignore 0.5 :depth -10 :local t)))
    '((:hook packlet-test-hook-a :function ignore :delay nil :depth 0 :local nil)
      (:hook packlet-test-hook-b :function ignore :delay nil :depth 90 :local nil)
      (:hook packlet-test-hook-c :function ignore :delay 0.5 :depth -10 :local t)))))

(ert-deftest packlet-test-normalize-hook-setqs ()
  (should
   (equal
    (packlet--normalize-hook-setqs
     '((packlet-test-hook-a
        (packlet-test-hook-setq-value 42)
        (packlet-test-hook-setq-other "ok"))))
    '((:kind :hook-setq
       :hook packlet-test-hook-a
       :function
       (lambda ()
         (setq-local packlet-test-hook-setq-value 42)
         (setq-local packlet-test-hook-setq-other "ok")
         nil)
       :delay nil
       :depth 0
       :local nil)))))

(ert-deftest packlet-test-normalize-hook-adds ()
  (should
   (equal
    (packlet--normalize-hook-adds
     '((packlet-test-hook-a packlet-test-hook-b ignore :local t)))
    '((:kind :hook-add
       :hook packlet-test-hook-a
       :function
       (lambda ()
         (add-hook 'packlet-test-hook-b 'ignore nil t))
       :delay nil
       :depth 0
       :local nil)))))

(ert-deftest packlet-test-normalize-hook-enables ()
  (should
   (equal
    (packlet--normalize-hook-enables
     '((packlet-test-hook-a packlet-test-hook-enable-mode)
       (packlet-test-hook-b packlet-test-hook-enable-mode 0)))
    '((:kind :hook-enable
       :hook packlet-test-hook-a
       :function
       (lambda ()
         (funcall 'packlet-test-hook-enable-mode 1))
       :delay nil
       :depth 0
       :local nil)
      (:kind :hook-enable
       :hook packlet-test-hook-b
       :function
       (lambda ()
         (funcall 'packlet-test-hook-enable-mode 0))
       :delay nil
       :depth 0
       :local nil)))))

(ert-deftest packlet-test-user-defined-keyword ()
  (defvar packlet-test-user-kw-result nil)
  (let ((packlet--user-keywords nil))
    (unwind-protect
        (progn
          (setq packlet-test-user-kw-result nil)
          (packlet-define-keyword :my-test-kw
            :normalize (lambda (forms)
                         (mapcar #'symbol-name
                                 (packlet--normalize-symbols forms :my-test-kw)))
            :expand (lambda (ctx forms)
                      (let ((feature (plist-get ctx :feature)))
                        (mapcar (lambda (name)
                                  `(push (cons ',feature ,name)
                                         packlet-test-user-kw-result))
                                forms))))
          (should (assq :my-test-kw packlet--user-keywords))
          (eval '(packlet packlet-test-user-kw-feature
                   :my-test-kw (alpha beta)))
          (should (equal (nreverse packlet-test-user-kw-result)
                         '((packlet-test-user-kw-feature . "alpha")
                           (packlet-test-user-kw-feature . "beta")))))
      (setq packlet-test-user-kw-result nil))))

(ert-deftest packlet-test-user-keywords-follow-section-order ()
  (defvar packlet-test-user-kw-order nil)
  (let ((packlet--user-keywords nil))
    (unwind-protect
        (progn
          (setq packlet-test-user-kw-order nil)
          (packlet-define-keyword :packlet-b
            :expand (lambda (_ctx _forms)
                      '((push :b packlet-test-user-kw-order))))
          (packlet-define-keyword :packlet-a
            :expand (lambda (_ctx _forms)
                      '((push :a packlet-test-user-kw-order))))
          (eval
           '(packlet packlet-test-user-kw-order-feature
              :packlet-a first
              :packlet-b second))
          (should (equal (nreverse packlet-test-user-kw-order)
                         '(:a :b))))
      (setq packlet-test-user-kw-order nil))))

(ert-deftest packlet-test-user-keyword-rejects-builtin ()
  (should-error (packlet-define-keyword :init
                  :expand #'ignore)
                :type 'error))

(provide 'packlet-test-parse)

;;; packlet-test-parse.el ends here
