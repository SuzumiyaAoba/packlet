;;; packlet-test-hooks.el --- Hook tests for packlet -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Tests for hook registration, delayed hooks, hook-setq, and hook-call behavior.

;;; Code:

(eval-and-compile
  (require 'packlet-test-support))

(defvar packlet-test-hook-count)
(defvar packlet-test-hook-add-count)
(defvar packlet-test-hook-enable-mode nil)
(defvar packlet-test-startup-enable-mode nil)

(declare-function packlet-test-startup-hook "packlet-test-multi-hook-feature" ())
(declare-function packlet-test-major-mode-hook "packlet-test-multi-hook-feature" ())

(define-minor-mode packlet-test-hook-enable-mode
  "Minor mode used by `:hook-enable' tests.")

(define-minor-mode packlet-test-startup-enable-mode
  "Minor mode used by `:startup-enable' tests.")

(defun packlet-test-hook-add-target ()
  "Increment `packlet-test-hook-add-count'."
  (setq packlet-test-hook-add-count
        (1+ packlet-test-hook-add-count)))

(defun packlet-test-hook-enable-counter (&optional step)
  "Increment `packlet-test-hook-count' by STEP."
  (setq packlet-test-hook-count
        (+ packlet-test-hook-count (or step 1))))

(ert-deftest packlet-test-multiple-hooks-register-and-run ()
  (let* ((directory (make-temp-file "packlet-test-" t))
         (load-path (cons directory load-path))
         (emacs-startup-hook nil)
         (after-change-major-mode-hook nil))
    (unwind-protect
        (progn
          (packlet-test--cleanup-feature 'packlet-test-multi-hook-feature)
          (packlet-test--cleanup-symbols
           '(packlet-test-startup-hook
             packlet-test-major-mode-hook
             packlet-test-startup-hook-ran
             packlet-test-major-mode-hook-ran))
          (packlet-test--write-feature
           directory
           'packlet-test-multi-hook-feature
           "(defvar packlet-test-startup-hook-ran nil)\n\
(defvar packlet-test-major-mode-hook-ran nil)\n\
(defun packlet-test-startup-hook ()\n\
  (setq packlet-test-startup-hook-ran t))\n\
(defun packlet-test-major-mode-hook ()\n\
  (setq packlet-test-major-mode-hook-ran t))")
          (packlet packlet-test-multi-hook-feature
            :hook ((emacs-startup-hook . packlet-test-startup-hook)
                   (after-change-major-mode-hook . packlet-test-major-mode-hook)))
          (run-hooks 'emacs-startup-hook)
          (run-hooks 'after-change-major-mode-hook)
          (should (bound-and-true-p packlet-test-startup-hook-ran))
          (should (bound-and-true-p packlet-test-major-mode-hook-ran)))
      (packlet-test--cleanup-feature 'packlet-test-multi-hook-feature)
      (packlet-test--cleanup-symbols
       '(packlet-test-startup-hook
         packlet-test-major-mode-hook
         packlet-test-startup-hook-ran
         packlet-test-major-mode-hook-ran))
      (delete-directory directory t))))

(ert-deftest packlet-test-delayed-hook-schedules-idle-timer ()
  "Delayed hook entries should schedule an idle timer instead of calling directly."
  (let* ((directory (make-temp-file "packlet-test-" t))
         (load-path (cons directory load-path))
         (scheduled nil)
         (timer-id 0))
    (unwind-protect
        (progn
          (packlet-test--cleanup-feature 'packlet-test-delayed-hook-feature)
          (packlet-test--cleanup-symbols
           '(packlet-test-delayed-fn packlet-test-delayed-ran))
          (defvar packlet-test-delayed-schedule-hook nil)
          (setq packlet-test-delayed-schedule-hook nil)
          (packlet-test--write-feature
           directory
           'packlet-test-delayed-hook-feature
           "(defvar packlet-test-delayed-ran nil)\n\
(defun packlet-test-delayed-fn ()\n\
  (setq packlet-test-delayed-ran t))")
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
                            (eq (car object) :timer)))))
            (eval
             `(packlet packlet-test-delayed-hook-feature
                :hook ((packlet-test-delayed-schedule-hook packlet-test-delayed-fn 2.0))))
            (run-hooks 'packlet-test-delayed-schedule-hook)
            (should (= (length scheduled) 1))
            (should (= (plist-get (car scheduled) :secs) 2.0))
            (packlet-test--invoke-scheduled-timer (car scheduled))
            (should (bound-and-true-p packlet-test-delayed-ran))))
      (packlet-test--cleanup-feature 'packlet-test-delayed-hook-feature)
      (packlet-test--cleanup-symbols
       '(packlet-test-delayed-fn packlet-test-delayed-ran
         packlet-test-delayed-schedule-hook))
      (delete-directory directory t))))

(ert-deftest packlet-test-delayed-hook-debounces ()
  "Delayed hook should cancel previous timer when hook fires again."
  (let* ((scheduled nil)
         (cancelled nil)
         (timer-id 0))
    (defvar packlet-test-debounce-hook nil)
    (setq packlet-test-debounce-hook nil)
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
                  ((symbol-function 'cancel-timer)
                   (lambda (timer)
                     (push timer cancelled))))
          (eval
           `(packlet packlet-test-debounce-feature
              :hook ((packlet-test-debounce-hook ignore 1.0))))
          (run-hooks 'packlet-test-debounce-hook)
          (should (= (length scheduled) 1))
          (run-hooks 'packlet-test-debounce-hook)
          (should (= (length cancelled) 1))
          (should (= (length scheduled) 2)))
      (packlet-test--cleanup-symbols '(packlet-test-debounce-hook)))))

(ert-deftest packlet-test-hook-options-control-order ()
  (defvar packlet-test-order-hook nil)
  (let ((packlet-test-order-hook nil)
        (result nil))
    (unwind-protect
        (progn
          (add-hook 'packlet-test-order-hook
                    (lambda ()
                      (setq result (append result '(middle)))))
          (packlet packlet-test-hook-order-feature
            :hook ((packlet-test-order-hook
                    (lambda ()
                      (setq result (append result '(first))))
                    :depth -10)
                   (packlet-test-order-hook
                    (lambda ()
                      (setq result (append result '(last))))
                    :append t)))
          (run-hooks 'packlet-test-order-hook)
          (should (equal result '(first middle last))))
      (setq packlet-test-order-hook nil))))

(ert-deftest packlet-test-hook-local-stays-buffer-local ()
  (defvar packlet-test-local-hook nil)
  (let ((buffer-a (generate-new-buffer " *packlet-local-a*"))
        (buffer-b (generate-new-buffer " *packlet-local-b*")))
    (unwind-protect
        (progn
          (setq packlet-test-hook-count 0
                packlet-test-local-hook nil)
          (with-current-buffer buffer-a
            (emacs-lisp-mode)
            (eval
             '(packlet packlet-test-local-hook-feature
                :hook ((packlet-test-local-hook
                        (lambda ()
                          (setq packlet-test-hook-count
                                (1+ packlet-test-hook-count)))
                        :local t)))))
          (with-current-buffer buffer-a
            (run-hooks 'packlet-test-local-hook))
          (with-current-buffer buffer-b
            (run-hooks 'packlet-test-local-hook))
          (should (= packlet-test-hook-count 1)))
      (when (buffer-live-p buffer-a)
        (kill-buffer buffer-a))
      (when (buffer-live-p buffer-b)
        (kill-buffer buffer-b))
      (setq packlet-test-hook-count nil)
      (packlet-test--cleanup-symbols '(packlet-test-local-hook)))))

(ert-deftest packlet-test-hook-reeval-does-not-duplicate ()
  (defvar packlet-test-reeval-hook nil)
  (unwind-protect
      (progn
        (setq packlet-test-hook-count 0
              packlet-test-reeval-hook nil)
        (eval
         '(packlet packlet-test-reeval-hook-feature
            :hook ((packlet-test-reeval-hook
                    . (lambda ()
                        (setq packlet-test-hook-count
                              (1+ packlet-test-hook-count)))))))
        (eval
         '(packlet packlet-test-reeval-hook-feature
            :hook ((packlet-test-reeval-hook
                    . (lambda ()
                        (setq packlet-test-hook-count
                              (1+ packlet-test-hook-count)))))))
        (run-hooks 'packlet-test-reeval-hook)
        (should (= packlet-test-hook-count 1)))
    (setq packlet-test-hook-count nil)
    (packlet-test--cleanup-symbols '(packlet-test-reeval-hook))))

(ert-deftest packlet-test-hook-reeval-after-entry-removal-clears-old-hook ()
  (defvar packlet-test-reeval-hook-a nil)
  (defvar packlet-test-reeval-hook-b nil)
  (let ((source-file (make-temp-file "packlet-test-hook-remove-" nil ".el")))
    (unwind-protect
        (progn
          (setq packlet-test-hook-count 0
                packlet-test-reeval-hook-a nil
                packlet-test-reeval-hook-b nil)
          (with-temp-buffer
            (emacs-lisp-mode)
            (setq buffer-file-name source-file)
            (insert "(packlet packlet-test-hook-remove-feature\n\
  :hook ((packlet-test-reeval-hook-a\n\
          . (lambda ()\n\
              (setq packlet-test-hook-count (1+ packlet-test-hook-count))))\n\
         (packlet-test-reeval-hook-b\n\
          . (lambda ()\n\
              (setq packlet-test-hook-count (+ packlet-test-hook-count 10))))))\n")
            (goto-char (point-min))
            (eval-buffer)
            (erase-buffer)
            (insert "(packlet packlet-test-hook-remove-feature\n\
  :hook ((packlet-test-reeval-hook-a\n\
          . (lambda ()\n\
              (setq packlet-test-hook-count (1+ packlet-test-hook-count))))))\n")
            (goto-char (point-min))
            (eval-buffer))
          (run-hooks 'packlet-test-reeval-hook-a)
          (run-hooks 'packlet-test-reeval-hook-b)
          (should (= packlet-test-hook-count 1))
          (should-not packlet-test-reeval-hook-b))
      (setq packlet-test-hook-count nil)
      (packlet-test--cleanup-symbols
       '(packlet-test-reeval-hook-a packlet-test-reeval-hook-b))
      (when (file-exists-p source-file)
        (delete-file source-file)))))

(ert-deftest packlet-test-hook-setq-applies-buffer-local-settings ()
  (defvar packlet-test-hook-setq-hook nil)
  (unwind-protect
      (progn
        (setq packlet-test-hook-setq-hook nil
              packlet-test-hook-setq-value nil
              packlet-test-hook-setq-other nil)
        (eval
         '(packlet packlet-test-hook-setq-feature
            :hook-setq ((packlet-test-hook-setq-hook
                         (packlet-test-hook-setq-value 42)
                         (packlet-test-hook-setq-other "ok")))))
        (with-temp-buffer
          (run-hooks 'packlet-test-hook-setq-hook)
          (should (local-variable-p 'packlet-test-hook-setq-value (current-buffer)))
          (should (local-variable-p 'packlet-test-hook-setq-other (current-buffer)))
          (should (equal packlet-test-hook-setq-value 42))
          (should (equal packlet-test-hook-setq-other "ok")))
        (should (null packlet-test-hook-setq-value))
        (should (null packlet-test-hook-setq-other)))
    (setq packlet-test-hook-setq-value nil
          packlet-test-hook-setq-other nil)
    (packlet-test--cleanup-symbols '(packlet-test-hook-setq-hook))))

(ert-deftest packlet-test-hook-setq-delay-preserves-hook-buffer ()
  (let ((scheduled nil)
        (timer-id 0))
    (defvar packlet-test-hook-setq-delayed-hook nil)
    (unwind-protect
        (progn
          (setq packlet-test-hook-setq-delayed-hook nil
                packlet-test-hook-setq-value nil)
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
                            (eq (car object) :timer)))))
            (let ((target (generate-new-buffer " *packlet-hook-setq-delay*"))
                  (other (generate-new-buffer " *packlet-hook-setq-delay-other*")))
              (unwind-protect
                  (progn
                    (eval
                     '(packlet packlet-test-hook-setq-delayed-feature
                        :hook-setq ((packlet-test-hook-setq-delayed-hook
                                     (packlet-test-hook-setq-value 42)
                                     :delay 0.5))))
                    (with-current-buffer target
                      (run-hooks 'packlet-test-hook-setq-delayed-hook)
                      (should-not (local-variable-p 'packlet-test-hook-setq-value)))
                    (with-current-buffer other
                      (packlet-test--invoke-scheduled-timer (car scheduled)))
                    (with-current-buffer target
                      (should (local-variable-p 'packlet-test-hook-setq-value))
                      (should (= packlet-test-hook-setq-value 42)))
                    (with-current-buffer other
                      (should-not (local-variable-p 'packlet-test-hook-setq-value))))
                (when (buffer-live-p target)
                  (kill-buffer target))
                (when (buffer-live-p other)
                  (kill-buffer other))))))
      (setq packlet-test-hook-setq-value nil)
      (packlet-test--cleanup-symbols '(packlet-test-hook-setq-delayed-hook)))))

(ert-deftest packlet-test-hook-setq-reeval-does-not-duplicate ()
  (defvar packlet-test-hook-setq-reeval-hook nil)
  (unwind-protect
      (progn
        (setq packlet-test-hook-setq-reeval-hook nil
              packlet-test-hook-count 0)
        (eval
         '(packlet packlet-test-hook-setq-reeval-feature
            :hook-setq ((packlet-test-hook-setq-reeval-hook
                         (packlet-test-hook-count
                          (1+ packlet-test-hook-count))))))
        (eval
         '(packlet packlet-test-hook-setq-reeval-feature
            :hook-setq ((packlet-test-hook-setq-reeval-hook
                         (packlet-test-hook-count
                          (1+ packlet-test-hook-count))))))
        (with-temp-buffer
          (run-hooks 'packlet-test-hook-setq-reeval-hook)
          (should (= packlet-test-hook-count 1))))
    (setq packlet-test-hook-count nil)
    (packlet-test--cleanup-symbols '(packlet-test-hook-setq-reeval-hook))))

(ert-deftest packlet-test-hook-call-runs-function-with-args ()
  (defvar packlet-test-hook-call-hook nil)
  (unwind-protect
      (progn
        (setq packlet-test-hook-call-hook nil
              packlet-test-hook-count 0)
        (eval
         '(packlet packlet-test-hook-call-feature
            :hook-call ((packlet-test-hook-call-hook
                         packlet-test-hook-enable-counter 2))))
        (run-hooks 'packlet-test-hook-call-hook)
        (should (= packlet-test-hook-count 2)))
    (setq packlet-test-hook-count nil)
    (packlet-test--cleanup-symbols '(packlet-test-hook-call-hook))))

(ert-deftest packlet-test-hook-add-registers-local-hook ()
  (defvar packlet-test-hook-add-source-hook nil)
  (defvar packlet-test-hook-add-target-hook nil)
  (unwind-protect
      (progn
        (setq packlet-test-hook-add-source-hook nil
              packlet-test-hook-add-target-hook nil
              packlet-test-hook-add-count 0)
        (eval
         '(packlet packlet-test-hook-add-feature
            :hook-add ((packlet-test-hook-add-source-hook
                        packlet-test-hook-add-target-hook
                        packlet-test-hook-add-target
                        :local t))))
        (with-temp-buffer
          (run-hooks 'packlet-test-hook-add-source-hook)
          (run-hooks 'packlet-test-hook-add-target-hook)
          (should (= packlet-test-hook-add-count 1)))
        (with-temp-buffer
          (run-hooks 'packlet-test-hook-add-target-hook)
          (should (= packlet-test-hook-add-count 1))))
    (setq packlet-test-hook-add-count nil)
    (packlet-test--cleanup-symbols
     '(packlet-test-hook-add-source-hook packlet-test-hook-add-target-hook))))

(ert-deftest packlet-test-hook-enable-runs-mode-like-function ()
  (defvar packlet-test-hook-enable-hook nil)
  (unwind-protect
      (progn
        (setq packlet-test-hook-enable-hook nil)
        (with-temp-buffer
          (setq packlet-test-hook-enable-mode nil)
          (eval
           '(packlet packlet-test-hook-enable-feature
              :hook-enable ((packlet-test-hook-enable-hook
                             packlet-test-hook-enable-mode))))
          (run-hooks 'packlet-test-hook-enable-hook)
          (should packlet-test-hook-enable-mode)))
    (packlet-test--cleanup-symbols '(packlet-test-hook-enable-hook))))

(ert-deftest packlet-test-hook-enable-reeval-does-not-duplicate ()
  (defvar packlet-test-hook-enable-reeval-hook nil)
  (unwind-protect
      (progn
        (setq packlet-test-hook-enable-reeval-hook nil
              packlet-test-hook-count 0)
        (eval
         '(packlet packlet-test-hook-enable-reeval-feature
            :hook-enable ((packlet-test-hook-enable-reeval-hook
                           packlet-test-hook-enable-counter))))
        (eval
         '(packlet packlet-test-hook-enable-reeval-feature
            :hook-enable ((packlet-test-hook-enable-reeval-hook
                           packlet-test-hook-enable-counter))))
        (run-hooks 'packlet-test-hook-enable-reeval-hook)
        (should (= packlet-test-hook-count 1)))
    (setq packlet-test-hook-count nil)
    (packlet-test--cleanup-symbols '(packlet-test-hook-enable-reeval-hook))))

(ert-deftest packlet-test-startup-runs-on-after-init-hook ()
  (let ((after-init-time nil)
        (after-init-hook nil))
    (setq packlet-test-hook-count 0)
    (packlet packlet-test-startup-feature
      :startup (packlet-test-hook-enable-counter 2))
    (should (= packlet-test-hook-count 0))
    (run-hooks 'after-init-hook)
    (should (= packlet-test-hook-count 2))
    (run-hooks 'after-init-hook)
    (should (= packlet-test-hook-count 2))
    (setq packlet-test-hook-count nil)))

(ert-deftest packlet-test-startup-runs-immediately-after-startup ()
  (let ((after-init-time (current-time))
        (after-init-hook nil))
    (setq packlet-test-hook-count 0)
    (packlet packlet-test-startup-immediate-feature
      :startup (packlet-test-hook-enable-counter 3))
    (should (= packlet-test-hook-count 3))
    (setq packlet-test-hook-count nil)))

(ert-deftest packlet-test-startup-enable-runs-on-after-init-hook ()
  (let ((after-init-time nil)
        (after-init-hook nil))
    (setq packlet-test-startup-enable-mode nil)
    (packlet packlet-test-startup-enable-feature
      :startup-enable packlet-test-startup-enable-mode)
    (should-not packlet-test-startup-enable-mode)
    (run-hooks 'after-init-hook)
    (should packlet-test-startup-enable-mode)
    (run-hooks 'after-init-hook)
    (should packlet-test-startup-enable-mode)
    (setq packlet-test-startup-enable-mode nil)))

(ert-deftest packlet-test-startup-enable-cleanup-restores-mode-state ()
  (let ((after-init-time (current-time))
        (after-init-hook nil))
    (with-temp-buffer
      (setq packlet-test-startup-enable-mode nil)
      (eval
       '(packlet packlet-test-startup-enable-cleanup-feature
          :startup-enable packlet-test-startup-enable-mode))
      (should packlet-test-startup-enable-mode)
      (packlet-cleanup-source (current-buffer))
      (should-not packlet-test-startup-enable-mode))))

(ert-deftest packlet-test-delayed-hook-reeval-does-not-duplicate ()
  (let* ((scheduled nil)
         (timer-id 0))
    (defvar packlet-test-delayed-reeval-hook nil)
    (unwind-protect
        (progn
          (setq packlet-test-hook-count 0
                packlet-test-delayed-reeval-hook nil)
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
                            (eq (car object) :timer)))))
            (eval
             '(packlet packlet-test-delayed-reeval-feature
                :hook ((packlet-test-delayed-reeval-hook
                        (lambda ()
                          (setq packlet-test-hook-count
                                (1+ packlet-test-hook-count)))
                        0.5))))
            (eval
             '(packlet packlet-test-delayed-reeval-feature
                :hook ((packlet-test-delayed-reeval-hook
                        (lambda ()
                          (setq packlet-test-hook-count
                                (1+ packlet-test-hook-count)))
                        0.5))))
            (run-hooks 'packlet-test-delayed-reeval-hook)
            (should (= (length scheduled) 1))
            (packlet-test--invoke-scheduled-timer (car scheduled))
            (should (= packlet-test-hook-count 1))))
      (setq packlet-test-hook-count nil)
      (packlet-test--cleanup-symbols '(packlet-test-delayed-reeval-hook)))))

(ert-deftest packlet-test-delayed-hook-rejects-negative-delay ()
  (should-error
   (eval
    '(packlet packlet-test-negative-delay
       :hook ((packlet-test-negative-delay-hook ignore -1.0))))
   :type 'error))

(provide 'packlet-test-hooks)

;;; packlet-test-hooks.el ends here
