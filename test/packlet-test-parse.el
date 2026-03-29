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

(ert-deftest packlet-test-file-form-validates-inputs ()
  (should (equal (packlet--file-form nil 'packlet-test-default-file)
                 "packlet-test-default-file"))
  (should (equal (packlet--file-form '((:file packlet-test-library))
                                     'packlet-test-default-file)
                 "packlet-test-library"))
  (should (equal (packlet--file-form '((:file "packlet-test-library"))
                                     'packlet-test-default-file)
                 "packlet-test-library"))
  (should-error (packlet--file-form '((:file))
                                    'packlet-test-default-file))
  (should-error (packlet--file-form '((:file 42))
                                    'packlet-test-default-file))
  (should-error (packlet--file-form '((:file packlet-test-library "extra"))
                                    'packlet-test-default-file)))

(ert-deftest packlet-test-demand-and-idle-forms-validate-arity ()
  (should (eq (packlet--demand-form '((:demand))) t))
  (should (equal (packlet--demand-form '((:demand (featurep 'foo))))
                 '(featurep 'foo)))
  (should-error (packlet--demand-form '((:demand t nil))))
  (should (eq (packlet--idle-form '((:idle))) t))
  (should (= (packlet--idle-form '((:idle 2.5))) 2.5))
  (should-error (packlet--idle-form '((:idle 1 2)))))

(ert-deftest packlet-test-when-and-unless-forms-validate-arity ()
  (should (equal (packlet--keyword-form '((:when (display-graphic-p))) :when)
                 '(display-graphic-p)))
  (should (equal (packlet--keyword-form '((:unless noninteractive)) :unless)
                 'noninteractive))
  (should-error (packlet--keyword-form '((:when)) :when))
  (should-error (packlet--keyword-form '((:unless a b)) :unless))
  (should (equal (packlet--guard-form '((:when foo) (:unless bar)))
                 '(and foo (not bar))))
  (should (equal (packlet--guard-form '((:unless bar)))
                 '(not bar)))
  (should-not (packlet--guard-form nil)))

(ert-deftest packlet-test-idle-delay-validates-values ()
  (should-not (packlet--idle-delay nil))
  (should (= (packlet--idle-delay t) 1.0))
  (should (= (packlet--idle-delay 0) 0))
  (should (= (packlet--idle-delay 2.5) 2.5))
  (should-error (packlet--idle-delay -1))
  (should-error (packlet--idle-delay 'later)))

(ert-deftest packlet-test-keyword-options-parse-and-split ()
  (let ((bool-p (lambda (value) (memq value '(nil t)))))
    (should
     (equal
      (packlet--parse-keyword-options
       '(:append t :depth -10)
       `((:append . ,bool-p)
         (:depth . numberp))
       "hook")
      '((:append . t) (:depth . -10))))
    (should
     (equal
      (packlet--split-trailing-keyword-options
       '(packlet-test-hook-enable-counter 2 :delay 0.5 :local t)
       '((:delay . numberp)
         (:local . nil))
       "hook-call")
      (cons '(packlet-test-hook-enable-counter 2)
            '(:delay 0.5 :local t))))
    (should-error
     (packlet--parse-keyword-options
      '(append t)
      `((:append . ,bool-p))
      "hook"))
    (should-error
     (packlet--parse-keyword-options
      '(:append)
      `((:append . ,bool-p))
      "hook"))
    (should-error
     (packlet--parse-keyword-options
      '(:bogus t)
      `((:append . ,bool-p))
      "hook"))
    (should-error
     (packlet--parse-keyword-options
      '(:append maybe)
      `((:append . ,bool-p))
      "hook"))))

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
        (packlet-test-hook-setq-other "ok")
        :delay 0.5
        :append t
        :local t)))
    '((:kind :hook-setq
       :hook packlet-test-hook-a
       :function
       (lambda ()
         (setq-local packlet-test-hook-setq-value 42)
         (setq-local packlet-test-hook-setq-other "ok")
         nil)
       :delay 0.5
       :depth 90
       :local t)))))

(ert-deftest packlet-test-normalize-hook-calls ()
  (should
   (equal
    (packlet--normalize-hook-calls
     '((packlet-test-hook-a
        packlet-test-hook-enable-counter 2
        :delay 0.5
        :append t
        :local t)))
    '((:kind :hook-call
       :hook packlet-test-hook-a
       :function
       (lambda ()
         (funcall 'packlet-test-hook-enable-counter 2))
       :autoload packlet-test-hook-enable-counter
       :delay 0.5
       :depth 90
       :local t)))))

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
       :autoload ignore
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
       :autoload packlet-test-hook-enable-mode
       :delay nil
       :depth 0
       :local nil)
      (:kind :hook-enable
       :hook packlet-test-hook-b
       :function
       (lambda ()
         (funcall 'packlet-test-hook-enable-mode 0))
       :autoload packlet-test-hook-enable-mode
       :delay nil
       :depth 0
       :local nil)))))

(ert-deftest packlet-test-normalize-startups ()
  (should
   (equal
    (packlet--normalize-startups
     '(packlet-test-hook-enable-counter
       (packlet-test-hook-enable-counter 2)
       ((packlet-test-hook-enable-counter 3))))
    '((:kind :startup
       :function packlet-test-hook-enable-counter
       :args nil)
      (:kind :startup
       :function packlet-test-hook-enable-counter
       :args (2))
      (:kind :startup
       :function packlet-test-hook-enable-counter
       :args (3))))))

(ert-deftest packlet-test-normalize-startup-enables ()
  (should
   (equal
    (packlet--normalize-startup-enables
     '(packlet-test-hook-enable-mode
       (packlet-test-hook-enable-mode 0)))
    '((:function packlet-test-hook-enable-mode
       :arg 1
       :kind :startup-enable)
      (:function packlet-test-hook-enable-mode
       :arg 0
       :kind :startup-enable)))))

(ert-deftest packlet-test-normalize-autoloads-accepts-mixed-forms ()
  (should
   (equal
    (packlet--normalize-autoloads
     '(packlet-test-autoload-a
       (packlet-test-autoload-b packlet-test-autoload-c)
       (packlet-test-autoload-d "packlet-test-d")
       ((packlet-test-autoload-e "packlet-test-e" t))))
    '((packlet-test-autoload-a nil nil)
      (packlet-test-autoload-b nil nil)
      (packlet-test-autoload-c nil nil)
      (packlet-test-autoload-d "packlet-test-d" nil)
      (packlet-test-autoload-e "packlet-test-e" t))))
  (should-error
   (packlet--normalize-autoloads
    '((42 "packlet-test-invalid")))))

(ert-deftest packlet-test-normalize-bindings-expands-map-groups ()
  (should
   (equal
    (packlet--normalize-bindings
     '((:map packlet-test-parent-map
        ([f5] . packlet-test-command)
        ("C-c p" . packlet-test-prefix-command))
       ("C-c z" . packlet-test-global-command)))
    '((:map packlet-test-parent-map [f5] packlet-test-command)
      (:map packlet-test-parent-map "C-c p" packlet-test-prefix-command)
      (:global "C-c z" packlet-test-global-command))))
  (should-error
   (packlet--normalize-bindings
    '((:map packlet-test-parent-map invalid)))))

(ert-deftest packlet-test-normalize-faces-accepts-copy-only-entry ()
  (should
   (equal
    (packlet--normalize-faces
     '((packlet-test-face :copy mode-line)))
    '((:face packlet-test-face :copy mode-line :attributes nil))))
  (should-error
   (packlet--normalize-faces
    '((packlet-test-face)))))

(ert-deftest packlet-test-normalize-derived-modes-preserves-docstring-and-body ()
  (should
   (equal
    (packlet--normalize-derived-modes
     '((packlet-test-derived text-mode "Derived"
        "Derived mode docstring."
        (setq-local fill-column 88)
        (setq-local tab-width 4))))
    '((:mode packlet-test-derived
       :parent text-mode
       :name "Derived"
       :docstring "Derived mode docstring."
       :body ((setq-local fill-column 88)
              (setq-local tab-width 4)))))))

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
