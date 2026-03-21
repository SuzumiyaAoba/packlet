;;; packlet.el --- Lazy-first package setup DSL -*- lexical-binding: t; -*-

;; Copyright (C) 2026 SuzumiyaAoba
;; Author: SuzumiyaAoba <SuzumiyaAoba@gmail.com>
;; Maintainer: SuzumiyaAoba <SuzumiyaAoba@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience, lisp
;; URL: https://github.com/SuzumiyaAoba/packlet
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; `packlet' provides a compact DSL for lazy-first package setup.
;;
;; Example:
;;
;;   (packlet magit
;;     :commands (magit-status)
;;     :bind ("C-x g" . magit-status)
;;     :after project
;;     :config
;;     (setq magit-display-buffer-function
;;           #'magit-display-buffer-same-window-except-diff-v1))

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defun packlet--all-features-loaded-p (features)
  "Return non-nil when every feature in FEATURES is loaded."
  (cl-every #'featurep features))

(defun packlet--maybe-autoload (function file &optional interactive)
  "Autoload FUNCTION from FILE when it is not already defined.
When INTERACTIVE is non-nil, register FUNCTION as an interactive command."
  (when (and (symbolp function)
             (not (fboundp function)))
    (autoload function file nil interactive)))

(defun packlet--register-after-load (feature function afters)
  "Run FUNCTION after FEATURE and AFTERS are loaded."
  (with-eval-after-load feature
    (funcall function))
  (dolist (after afters)
    (with-eval-after-load after
      (funcall function)))
  (funcall function))

(defun packlet--register-demand-load (feature afters)
  "Require FEATURE once AFTERS are loaded."
  (let ((load-now
         (lambda ()
           (when (and (not (featurep feature))
                      (packlet--all-features-loaded-p afters))
             (require feature nil t)))))
    (dolist (after afters)
      (with-eval-after-load after
        (funcall load-now)))
    (funcall load-now)))

(eval-and-compile
  (defconst packlet--keywords
    '(:init :config :commands :mode :hook :bind :after :demand))

  (defun packlet--keyword-p (form)
    "Return non-nil when FORM is a supported `packlet' keyword."
    (memq form packlet--keywords))

  (defun packlet--proper-list-p (value)
    "Return non-nil when VALUE is a proper list."
    (while (consp value)
      (setq value (cdr value)))
    (null value))

  (defun packlet--parse-body (body)
    "Parse packlet BODY into an alist keyed by DSL keyword."
    (let (sections current)
      (dolist (form body)
        (if (packlet--keyword-p form)
            (progn
              (setq current form)
              (unless (assq current sections)
                (push (cons current nil) sections)))
          (unless current
            (error "packlet: %S is not under a keyword" form))
          (setcdr (assq current sections)
                  (append (cdr (assq current sections))
                          (list form)))))
      (nreverse sections)))

  (defun packlet--section (sections keyword)
    "Return forms from SECTIONS for KEYWORD."
    (cdr (assq keyword sections)))

  (defun packlet--has-section-p (sections keyword)
    "Return non-nil when SECTIONS contains KEYWORD."
    (assq keyword sections))

  (defun packlet--normalize-symbols (forms keyword)
    "Normalize FORMS under KEYWORD into a flat list of symbols."
    (let (result)
      (dolist (form forms)
        (cond
         ((null form))
         ((symbolp form)
          (push form result))
         ((packlet--proper-list-p form)
          (dolist (item form)
            (unless (symbolp item)
              (error "packlet: %S under %S must be a symbol" item keyword))
            (push item result)))
         (t
          (error "packlet: invalid value %S for %S" form keyword))))
      (nreverse result)))

  (defun packlet--mode-entry-p (value)
    "Return non-nil when VALUE is a valid `:mode' entry."
    (and (consp value)
         (stringp (car value))
         (symbolp (cdr value))))

  (defun packlet--hook-entry-p (value)
    "Return non-nil when VALUE is a valid `:hook' entry."
    (and (consp value)
         (symbolp (car value))
         (let ((function (cdr value)))
           (or (symbolp function)
               (and (consp function)
                    (eq (car function) 'lambda))))))

  (defun packlet--bind-entry-p (value)
    "Return non-nil when VALUE is a valid `:bind' entry."
    (and (consp value)
         (or (stringp (car value))
             (vectorp (car value)))
         (symbolp (cdr value))))

  (defun packlet--normalize-pairs (forms keyword predicate)
    "Normalize FORMS under KEYWORD into a flat list of pairs using PREDICATE."
    (let (result)
      (dolist (form forms)
        (cond
         ((funcall predicate form)
          (push form result))
         ((packlet--proper-list-p form)
          (dolist (entry form)
            (unless (funcall predicate entry)
              (error "packlet: invalid entry %S for %S" entry keyword))
            (push entry result)))
         (t
          (error "packlet: invalid value %S for %S" form keyword))))
      (nreverse result)))

  (defun packlet--demand-form (sections)
    "Return the `:demand' form from SECTIONS."
    (when-let ((entry (assq :demand sections)))
      (pcase (cdr entry)
        ('() t)
        (`(,form) form)
        (_ (error "packlet: :demand accepts at most one form")))))

  (defun packlet--hook-function-form (function)
    "Return a function form for FUNCTION suitable for `add-hook'."
    `(function ,function)))

;;;###autoload
(defmacro packlet (feature &rest body)
  "Define lazy-first setup for FEATURE."
  (declare (indent 1))
  (unless (symbolp feature)
    (error "packlet: FEATURE must be a symbol"))
  (let* ((sections (packlet--parse-body body))
         (init-forms (packlet--section sections :init))
         (config-forms (packlet--section sections :config))
         (commands (packlet--normalize-symbols
                    (packlet--section sections :commands)
                    :commands))
         (modes (packlet--normalize-pairs
                 (packlet--section sections :mode)
                 :mode
                 #'packlet--mode-entry-p))
         (hooks (packlet--normalize-pairs
                 (packlet--section sections :hook)
                 :hook
                 #'packlet--hook-entry-p))
         (bindings (packlet--normalize-pairs
                    (packlet--section sections :bind)
                    :bind
                    #'packlet--bind-entry-p))
         (afters (packlet--normalize-symbols
                  (packlet--section sections :after)
                  :after))
         (demand-form (packlet--demand-form sections))
         (file (symbol-name feature))
         (configured-var (intern (format "packlet--configured-%s" feature)))
         (config-function (intern (format "packlet--configure-%s" feature))))
    `(progn
       ,@init-forms
       ,@(mapcar
          (lambda (command)
            `(packlet--maybe-autoload ',command ,file t))
          commands)
       ,@(mapcar
          (lambda (mode)
            `(progn
               (packlet--maybe-autoload ',(cdr mode) ,file t)
               (add-to-list 'auto-mode-alist ',mode)))
          modes)
       ,@(mapcar
          (lambda (hook)
            `(progn
               ,(when (symbolp (cdr hook))
                  `(packlet--maybe-autoload ',(cdr hook) ,file nil))
               (add-hook ',(car hook)
                         ,(packlet--hook-function-form (cdr hook)))))
          hooks)
       ,@(mapcar
          (lambda (binding)
            `(progn
               (packlet--maybe-autoload ',(cdr binding) ,file t)
               (global-set-key
                ,(if (stringp (car binding))
                     `(kbd ,(car binding))
                   (car binding))
                ',(cdr binding))))
          bindings)
       ,@(when config-forms
           `((defvar ,configured-var nil)
             (defun ,config-function ()
               ,(format "Apply deferred configuration for `%s'." feature)
               (when (and (not ,configured-var)
                          (featurep ',feature)
                          (packlet--all-features-loaded-p ',afters))
                 (setq ,configured-var t)
                 ,@config-forms))
             (packlet--register-after-load
              ',feature
              (function ,config-function)
              ',afters)))
       ,@(when (packlet--has-section-p sections :demand)
           `((when ,demand-form
               (packlet--register-demand-load ',feature ',afters)))))))

(provide 'packlet)

;;; packlet.el ends here
