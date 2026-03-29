;;; packlet-test-loading.el --- Loading and config tests for packlet -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Tests for config handlers, eager and idle loading, and after-load behavior.

;;; Code:

(eval-and-compile
  (require 'packlet-test-support))

(defvar packlet-test-reeval-result)

(ert-deftest packlet-test-demand-load-after-feature ()
  (let* ((directory (make-temp-file "packlet-test-" t))
         (load-path (cons directory load-path)))
    (unwind-protect
        (progn
          (packlet-test--cleanup-feature 'packlet-test-demand)
          (packlet-test--cleanup-feature 'packlet-test-after)
          (packlet-test--write-feature
           directory
           'packlet-test-after
           "(defvar packlet-test-after-loaded t)")
          (packlet-test--write-feature
           directory
           'packlet-test-demand
           "(defvar packlet-test-demand-loaded t)")
          (packlet packlet-test-demand
            :after packlet-test-after
            :demand t)
          (should-not (featurep 'packlet-test-demand))
          (require 'packlet-test-after)
          (should (featurep 'packlet-test-demand)))
      (packlet-test--cleanup-feature 'packlet-test-demand)
      (packlet-test--cleanup-feature 'packlet-test-after)
      (delete-directory directory t))))

(ert-deftest packlet-test-multiple-packlet-configs-run-for-same-feature ()
  (let* ((directory (make-temp-file "packlet-test-" t))
         (load-path (cons directory load-path))
         (config-a-ran nil)
         (config-b-ran nil))
    (unwind-protect
        (progn
          (packlet-test--cleanup-feature 'packlet-test-shared-config)
          (packlet-test--write-feature
           directory
           'packlet-test-shared-config
           "(defvar packlet-test-shared-config-loaded t)")
          (packlet packlet-test-shared-config
            :config
            (setq config-a-ran t))
          (packlet packlet-test-shared-config
            :config
            (setq config-b-ran t))
          (should-not config-a-ran)
          (should-not config-b-ran)
          (require 'packlet-test-shared-config)
          (should config-a-ran)
          (should config-b-ran))
      (packlet-test--cleanup-feature 'packlet-test-shared-config)
      (delete-directory directory t))))

(ert-deftest packlet-test-config-handlers-run-in-registration-order ()
  (let* ((directory (make-temp-file "packlet-test-" t))
         (load-path (cons directory load-path))
         (feature 'packlet-test-config-order)
         (result nil))
    (unwind-protect
        (progn
          (packlet-test--cleanup-feature feature)
          (packlet-test--write-feature
           directory
           feature
           "(defvar packlet-test-config-order-loaded t)")
          (packlet packlet-test-config-order
            :config
            (setq result (append result '(a))))
          (packlet packlet-test-config-order
            :config
            (setq result (append result '(b))))
          (require feature)
          (should (equal result '(a b))))
      (packlet-test--cleanup-feature feature)
      (delete-directory directory t))))

(ert-deftest packlet-test-config-reeval-does-not-duplicate ()
  (let* ((directory (make-temp-file "packlet-test-" t))
         (load-path (cons directory load-path))
         (feature 'packlet-test-reeval-config))
    (unwind-protect
        (progn
          (setq packlet-test-reeval-result nil)
          (packlet-test--cleanup-feature feature)
          (packlet-test--write-feature
           directory
           feature
           "(defvar packlet-test-reeval-config-loaded t)")
          (eval
           '(packlet packlet-test-reeval-config
              :config
              (push 'ran packlet-test-reeval-result)))
          (eval
           '(packlet packlet-test-reeval-config
              :config
              (push 'ran packlet-test-reeval-result)))
          (should-not packlet-test-reeval-result)
          (require feature)
          (should (equal packlet-test-reeval-result '(ran))))
      (setq packlet-test-reeval-result nil)
      (packlet-test--cleanup-feature feature)
      (delete-directory directory t))))

(ert-deftest packlet-test-config-reeval-updates-same-site ()
  (let* ((directory (make-temp-file "packlet-test-" t))
         (load-path (cons directory load-path))
         (feature 'packlet-test-reeval-buffer)
         (source-file (expand-file-name "packlet-test-reeval-buffer.el" directory)))
    (unwind-protect
        (progn
          (setq packlet-test-reeval-result nil)
          (packlet-test--cleanup-feature feature)
          (packlet-test--write-feature
           directory
           feature
           "(defvar packlet-test-reeval-buffer-loaded t)")
          (with-temp-buffer
            (emacs-lisp-mode)
            (setq buffer-file-name source-file)
            (insert "(packlet packlet-test-reeval-buffer\n\
  :config\n\
  (push 'old packlet-test-reeval-result))\n")
            (goto-char (point-min))
            (eval-buffer)
            (erase-buffer)
            (insert "(packlet packlet-test-reeval-buffer\n\
  :config\n\
  (push 'new packlet-test-reeval-result))\n")
            (goto-char (point-min))
            (eval-buffer))
          (require feature)
          (should (equal packlet-test-reeval-result '(new))))
      (setq packlet-test-reeval-result nil)
      (packlet-test--cleanup-feature feature)
      (delete-directory directory t))))

(ert-deftest packlet-test-multiple-packlets-in-same-top-level-form-do-not-collide ()
  (let* ((directory (make-temp-file "packlet-test-" t))
         (load-path (cons directory load-path))
         (feature 'packlet-test-shared-top-level))
    (unwind-protect
        (progn
          (setq packlet-test-reeval-result nil)
          (packlet-test--cleanup-feature feature)
          (packlet-test--write-feature
           directory
           feature
           "(defvar packlet-test-shared-top-level-loaded t)")
          (with-temp-buffer
            (emacs-lisp-mode)
            (setq buffer-file-name
                  (expand-file-name "packlet-test-shared-top-level-config.el"
                                    directory))
            (insert "(progn\n\
  (packlet packlet-test-shared-top-level\n\
    :config\n\
    (push 'a packlet-test-reeval-result))\n\
  (packlet packlet-test-shared-top-level\n\
    :config\n\
    (push 'b packlet-test-reeval-result)))\n")
            (goto-char (point-min))
            (eval-buffer))
          (require feature)
          (should (equal (sort (copy-sequence packlet-test-reeval-result)
                               (lambda (left right)
                                 (string-lessp (symbol-name left)
                                               (symbol-name right))))
                         '(a b))))
      (setq packlet-test-reeval-result nil)
      (packlet-test--cleanup-feature feature)
      (delete-directory directory t))))

(ert-deftest packlet-test-config-reeval-after-form-move-does-not-leak-old-site ()
  (let* ((directory (make-temp-file "packlet-test-" t))
         (load-path (cons directory load-path))
         (feature 'packlet-test-reeval-moved)
         (source-file (expand-file-name "packlet-test-reeval-moved.el" directory)))
    (unwind-protect
        (progn
          (setq packlet-test-reeval-result nil)
          (packlet-test--cleanup-feature feature)
          (packlet-test--write-feature
           directory
           feature
           "(defvar packlet-test-reeval-moved-loaded t)")
          (with-temp-buffer
            (emacs-lisp-mode)
            (setq buffer-file-name source-file)
            (insert "(packlet packlet-test-reeval-moved\n\
  :config\n\
  (push 'old packlet-test-reeval-result))\n")
            (goto-char (point-min))
            (eval-buffer)
            (erase-buffer)
            (insert "\n(packlet packlet-test-reeval-moved\n\
  :config\n\
  (push 'new packlet-test-reeval-result))\n")
            (goto-char (point-min))
            (eval-buffer))
          (require feature)
          (should (equal packlet-test-reeval-result '(new))))
      (setq packlet-test-reeval-result nil)
      (packlet-test--cleanup-feature feature)
      (delete-directory directory t))))

(ert-deftest packlet-test-config-reeval-after-form-removal-clears-old-site ()
  (let* ((directory (make-temp-file "packlet-test-" t))
         (load-path (cons directory load-path))
         (feature 'packlet-test-reeval-removed)
         (source-file (expand-file-name "packlet-test-reeval-removed.el" directory)))
    (unwind-protect
        (progn
          (setq packlet-test-reeval-result nil)
          (packlet-test--cleanup-feature feature)
          (packlet-test--write-feature
           directory
           feature
           "(defvar packlet-test-reeval-removed-loaded t)")
          (with-temp-buffer
            (emacs-lisp-mode)
            (setq buffer-file-name source-file)
            (insert "(packlet packlet-test-reeval-removed\n\
  :config\n\
  (push 'old packlet-test-reeval-result))\n")
            (goto-char (point-min))
            (eval-buffer)
            (erase-buffer)
            (insert ";; removed\n")
            (goto-char (point-min))
            (eval-buffer))
          (require feature)
          (should-not packlet-test-reeval-result))
      (setq packlet-test-reeval-result nil)
      (packlet-test--cleanup-feature feature)
      (delete-directory directory t))))

(ert-deftest packlet-test-idle-keyword-registers-loader ()
  (let (calls)
    (cl-letf (((symbol-function 'packlet--register-idle-load)
               (lambda (id feature afters file delay source-file)
                 (push (list id feature afters file delay source-file) calls))))
      (packlet packlet-test-idle-default
        :idle)
      (packlet packlet-test-idle-delayed
        :after packlet-test-after
        :idle 2.5))
    (should
     (equal (mapcar (lambda (call) (cdr (butlast call))) (nreverse calls))
            '((packlet-test-idle-default nil "packlet-test-idle-default" 1.0)
              (packlet-test-idle-delayed
               (packlet-test-after)
               "packlet-test-idle-delayed"
               2.5))))))

(ert-deftest packlet-test-idle-load-waits-for-startup ()
  (let ((after-init-time nil)
        (emacs-startup-hook nil)
        (scheduled nil)
        (load-count 0)
        (timer-id 0))
    (cl-letf (((symbol-function 'run-with-idle-timer)
               (lambda (secs repeat fn &rest args)
                 (let ((timer (list :timer (cl-incf timer-id))))
                   (push (list :timer timer :secs secs :repeat repeat
                               :fn fn :args args)
                         scheduled)
                   timer)))
              ((symbol-function 'timerp)
               (lambda (object)
                 (and (consp object)
                      (eq (car object) :timer))))
              ((symbol-function 'packlet--idle-load-ready-p)
               (lambda ()
                 t))
              ((symbol-function 'packlet--load-feature)
               (lambda (_feature _file)
                 (setq load-count (1+ load-count))
                 t)))
      (packlet--register-idle-load
       'packlet-test-idle-loader
       'packlet-test-idle
       nil
       "packlet-test-idle"
       2.5)
      (should-not scheduled)
      (run-hooks 'emacs-startup-hook)
      (should (= (length scheduled) 1))
      (should (= (plist-get (car scheduled) :secs) 2.5))
      (packlet-test--invoke-scheduled-timer (car scheduled))
      (should (= load-count 1)))))

(ert-deftest packlet-test-idle-load-retries-while-busy ()
  (let ((after-init-time (current-time))
        (scheduled nil)
        (load-count 0)
        (timer-id 0)
        (ready-values '(nil t)))
    (cl-letf (((symbol-function 'run-with-idle-timer)
               (lambda (secs repeat fn &rest args)
                 (let ((timer (list :timer (cl-incf timer-id))))
                   (push (list :timer timer :secs secs :repeat repeat
                               :fn fn :args args)
                         scheduled)
                   timer)))
              ((symbol-function 'timerp)
               (lambda (object)
                 (and (consp object)
                      (eq (car object) :timer))))
              ((symbol-function 'packlet--idle-load-ready-p)
               (lambda ()
                 (prog1 (car ready-values)
                   (setq ready-values
                         (or (cdr ready-values) ready-values)))))
              ((symbol-function 'packlet--load-feature)
               (lambda (_feature _file)
                 (setq load-count (1+ load-count))
                 t)))
      (packlet--register-idle-load
       'packlet-test-idle-loader
       'packlet-test-idle
       nil
       "packlet-test-idle"
       1.0)
      (should (= (length scheduled) 1))
      (packlet-test--invoke-scheduled-timer (car scheduled))
      (should (= load-count 0))
      (should (= (length scheduled) 2))
      (packlet-test--invoke-scheduled-timer (car scheduled))
      (should (= load-count 1)))))

(ert-deftest packlet-test-idle-load-waits-for-after-feature ()
  (let* ((directory (make-temp-file "packlet-test-" t))
         (load-path (cons directory load-path))
         (after-init-time (current-time))
         (scheduled nil)
         (timer-id 0))
    (unwind-protect
        (cl-letf (((symbol-function 'run-with-idle-timer)
                   (lambda (secs repeat fn &rest args)
                     (let ((timer (list :timer (cl-incf timer-id))))
                       (push (list :timer timer :secs secs :repeat repeat
                                   :fn fn :args args)
                             scheduled)
                       timer)))
                  ((symbol-function 'timerp)
                   (lambda (object)
                     (and (consp object)
                          (eq (car object) :timer))))
                  ((symbol-function 'packlet--idle-load-ready-p)
                   (lambda ()
                     t))
                  ((symbol-function 'packlet--load-feature)
                   (lambda (_feature _file)
                     t)))
          (packlet-test--cleanup-feature 'packlet-test-idle-after)
          (packlet-test--write-feature
           directory
           'packlet-test-idle-after
           "(defvar packlet-test-idle-after-loaded t)")
          (packlet--register-idle-load
           'packlet-test-idle-loader
           'packlet-test-idle
           '(packlet-test-idle-after)
           "packlet-test-idle"
           1.0)
          (should-not scheduled)
          (require 'packlet-test-idle-after)
          (should (= (length scheduled) 1)))
      (packlet-test--cleanup-feature 'packlet-test-idle-after)
      (delete-directory directory t))))

(ert-deftest packlet-test-file-demand-load ()
  (let* ((directory (make-temp-file "packlet-test-" t))
         (load-path (cons directory load-path))
         (feature 'packlet-test-demand-feature)
         (file 'packlet-test-demand-library))
    (unwind-protect
        (progn
          (packlet-test--cleanup-feature feature)
          (packlet-test--write-library
           directory
           file
           feature
           "(defvar packlet-test-demand-file-loaded t)")
          (should-not (featurep feature))
          (eval
           `(packlet ,feature
              :file ,file
              :demand t))
          (should (featurep feature)))
      (packlet-test--cleanup-feature feature)
      (delete-directory directory t))))

(ert-deftest packlet-test-load-keyword ()
  (let* ((directory (make-temp-file "packlet-test-" t))
         (load-path (cons directory load-path)))
    (unwind-protect
        (progn
          (packlet-test--write-file
           (expand-file-name "packlet-test-loadable.el" directory)
           "(defvar packlet-test-loaded-marker t)")
          (should-not (boundp 'packlet-test-loaded-marker))
          (packlet packlet-test-load-feature
            :load "packlet-test-loadable")
          (should (bound-and-true-p packlet-test-loaded-marker)))
      (when (boundp 'packlet-test-loaded-marker)
        (makunbound 'packlet-test-loaded-marker))
      (delete-directory directory t))))

(ert-deftest packlet-test-load-keyword-warns-when-library-missing ()
  (let (warnings)
    (cl-letf (((symbol-function 'display-warning)
               (lambda (_type message &optional _level)
                 (push message warnings))))
      (packlet packlet-test-missing-load
        :load "packlet-test-missing-library"))
    (should (= (length warnings) 1))
    (should (string-match-p
             "packlet-test-missing-library"
             (car warnings)))))

(ert-deftest packlet-test-demand-keyword-warns-when-library-missing ()
  (let (warnings)
    (cl-letf (((symbol-function 'display-warning)
               (lambda (_type message &optional _level)
                 (push message warnings))))
      (packlet packlet-test-missing-demand
        :demand t))
    (should (= (length warnings) 1))
    (should (string-match-p
             "packlet-test-missing-demand"
             (car warnings)))))

(ert-deftest packlet-test-after-load-keyword ()
  (let* ((directory (make-temp-file "packlet-test-" t))
         (load-path (cons directory load-path))
         (after-load-result nil))
    (unwind-protect
        (progn
          (packlet-test--cleanup-feature 'packlet-test-after-load-dep)
          (packlet-test--write-feature
           directory
           'packlet-test-after-load-dep
           "(defvar packlet-test-after-load-dep-loaded t)")
          (packlet packlet-test-after-load-feature
            :after-load
            (packlet-test-after-load-dep
             (setq after-load-result t)))
          (should-not after-load-result)
          (require 'packlet-test-after-load-dep)
          (should after-load-result))
      (packlet-test--cleanup-feature 'packlet-test-after-load-dep)
      (delete-directory directory t))))

(ert-deftest packlet-test-after-load-dispatcher-clears-when-last-handler-removed ()
  (let ((feature 'packlet-test-dispatcher-clear))
    (unwind-protect
        (progn
          (setq after-load-alist nil)
          (clrhash packlet--after-load-handlers)
          (clrhash packlet--after-load-dispatchers)
          (packlet--register-after-load-handler
           feature
           'first
           #'ignore)
          (should (gethash feature packlet--after-load-dispatchers))
          (should (packlet--after-load-handler-entries feature))
          (should (assq feature after-load-alist))
          (packlet--unregister-after-load-handler feature 'first)
          (should-not (packlet--after-load-handler-entries feature))
          (should-not (gethash feature packlet--after-load-dispatchers))
          (should-not (assq feature after-load-alist)))
      (clrhash packlet--after-load-handlers)
      (clrhash packlet--after-load-dispatchers)
      (setq after-load-alist nil))))

(provide 'packlet-test-loading)

;;; packlet-test-loading.el ends here
