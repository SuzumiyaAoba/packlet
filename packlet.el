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
    (packlet--maybe-load-autoloads file)
    (unless (fboundp function)
      (autoload function file nil interactive))))

(defvar packlet--autoloads-attempted (make-hash-table :test #'equal)
  "Autoload libraries that `packlet' has already tried to load.")

(defun packlet--autoloads-file (file)
  "Return the autoload library name for FILE."
  (format "%s-autoloads" file))

(defun packlet--maybe-load-autoloads (file)
  "Load FILE's autoload definitions once when available."
  (let ((autoloads-file (packlet--autoloads-file file)))
    (unless (gethash autoloads-file packlet--autoloads-attempted)
      (puthash autoloads-file t packlet--autoloads-attempted)
      (load autoloads-file t t))))

(defun packlet--load-feature (feature &optional file)
  "Load FEATURE, falling back to FILE when needed."
  (or (require feature nil t)
      (when file
        (load file nil t))))

(defun packlet--register-after-load (feature function afters)
  "Run FUNCTION after FEATURE and AFTERS are loaded."
  (with-eval-after-load feature
    (funcall function))
  (dolist (after afters)
    (with-eval-after-load after
      (funcall function)))
  (funcall function))

(defun packlet--register-demand-load (feature afters &optional file)
  "Load FEATURE once AFTERS are loaded.
When FILE is non-nil, use it as a fallback library name."
  (let ((load-now
         (lambda ()
           (when (and (not (featurep feature))
                      (packlet--all-features-loaded-p afters))
             (packlet--load-feature feature file)))))
    (dolist (after afters)
      (with-eval-after-load after
        (funcall load-now)))
    (funcall load-now)))

(defun packlet--idle-load-ready-p ()
  "Return non-nil when an idle load should proceed."
  (and (not (input-pending-p))
       (not (active-minibuffer-window))
       (not executing-kbd-macro)))

(defun packlet--register-idle-load (feature afters &optional file delay)
  "Load FEATURE after startup during idle time.
AFTERS must be loaded before scheduling begins.  When FILE is non-nil,
use it as a fallback library name.  DELAY is the idle duration in seconds."
  (let ((delay (or delay 1.0))
        (startup-complete after-init-time)
        timer
        startup-hook-fn
        maybe-schedule)
    (setq maybe-schedule
          (lambda ()
            (when (and startup-complete
                       (not (featurep feature))
                       (packlet--all-features-loaded-p afters)
                       (not (timerp timer)))
              (setq timer
                    (run-with-idle-timer
                     delay nil
                     (lambda ()
                       (setq timer nil)
                       (when (and startup-complete
                                  (not (featurep feature))
                                  (packlet--all-features-loaded-p afters))
                         (if (packlet--idle-load-ready-p)
                             (packlet--load-feature feature file)
                           (funcall maybe-schedule)))))))))
    (unless startup-complete
      (setq startup-hook-fn
            (lambda ()
              (setq startup-complete t)
              (remove-hook 'emacs-startup-hook startup-hook-fn)
              (funcall maybe-schedule)))
      (add-hook 'emacs-startup-hook startup-hook-fn))
    (dolist (after afters)
      (with-eval-after-load after
        (funcall maybe-schedule)))
    (funcall maybe-schedule)))

(defun packlet--resolve-keymap (keymap)
  "Return the keymap named by KEYMAP, or nil when it is not yet bound."
  (when (boundp keymap)
    (let ((value (symbol-value keymap)))
      (unless (keymapp value)
        (error "packlet: %S does not name a keymap" keymap))
      value)))

(defun packlet--register-keymap-binding (feature keymap key command afters)
  "Bind KEY to COMMAND in KEYMAP when the map becomes available.
FEATURE and AFTERS are watched for opportunities to install the binding."
  (let ((install
         (lambda ()
           (when-let ((map (packlet--resolve-keymap keymap)))
             (define-key map key command)))))
    (with-eval-after-load feature
      (funcall install))
    (dolist (after afters)
      (with-eval-after-load after
        (funcall install)))
    (funcall install)))

(eval-and-compile
  (defconst packlet--keywords
    '(:file :init :custom :config :commands :autoload :mode :hook :bind
            :after :idle :demand :functions :defines))

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

  (defun packlet--file-form (sections feature)
    "Return the library name from SECTIONS for FEATURE."
    (if-let ((entry (assq :file sections)))
        (pcase (cdr entry)
          ('()
           (error "packlet: :file requires a symbol or string"))
          (`(,form)
           (cond
            ((symbolp form)
             (symbol-name form))
            ((stringp form)
             form)
            (t
             (error "packlet: :file must be a symbol or string, got %S" form))))
          (_
           (error "packlet: :file accepts at most one form")))
      (symbol-name feature)))

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

  (defun packlet--bind-map-group-p (value)
    "Return non-nil when VALUE is a valid keymap binding group."
    (and (packlet--proper-list-p value)
         (eq (car value) :map)
         (symbolp (cadr value))
         (cddr value)))

  (defun packlet--custom-entry-p (value)
    "Return non-nil when VALUE is a valid `:custom' entry."
    (and (consp value)
         (symbolp (car value))
         (consp (cdr value))
         (null (cddr value))))

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

  (defun packlet--normalize-bindings (forms)
    "Normalize FORMS under `:bind'."
    (let (result)
      (dolist (form forms)
        (cond
         ((packlet--bind-entry-p form)
          (push (list :global (car form) (cdr form)) result))
         ((packlet--bind-map-group-p form)
          (let ((keymap (cadr form)))
            (dolist (entry (cddr form))
              (unless (packlet--bind-entry-p entry)
                (error "packlet: invalid entry %S for :bind" entry))
              (push (list :map keymap (car entry) (cdr entry)) result))))
         ((packlet--proper-list-p form)
          (dolist (entry form)
            (cond
             ((packlet--bind-entry-p entry)
              (push (list :global (car entry) (cdr entry)) result))
             ((packlet--bind-map-group-p entry)
              (let ((keymap (cadr entry)))
                (dolist (binding (cddr entry))
                  (unless (packlet--bind-entry-p binding)
                    (error "packlet: invalid entry %S for :bind" binding))
                  (push (list :map keymap (car binding) (cdr binding)) result))))
             (t
              (error "packlet: invalid entry %S for :bind" entry)))))
         (t
          (error "packlet: invalid value %S for :bind" form))))
      (nreverse result)))

  (defun packlet--normalize-customs (forms)
    "Normalize FORMS under `:custom'."
    (let (result)
      (dolist (form forms)
        (cond
         ((packlet--custom-entry-p form)
          (push form result))
         ((packlet--proper-list-p form)
          (dolist (entry form)
            (unless (packlet--custom-entry-p entry)
              (error "packlet: invalid entry %S for :custom" entry))
            (push entry result)))
         (t
          (error "packlet: invalid value %S for :custom" form))))
      (nreverse result)))

  (defun packlet--demand-form (sections)
    "Return the `:demand' form from SECTIONS."
    (when-let ((entry (assq :demand sections)))
      (pcase (cdr entry)
        ('() t)
        (`(,form) form)
        (_ (error "packlet: :demand accepts at most one form")))))

  (defun packlet--idle-form (sections)
    "Return the `:idle' form from SECTIONS."
    (when-let ((entry (assq :idle sections)))
      (pcase (cdr entry)
        ('() t)
        (`(,form) form)
        (_ (error "packlet: :idle accepts at most one form")))))

  (defun packlet--idle-delay (value)
    "Normalize VALUE for `:idle'."
    (cond
     ((null value) nil)
     ((eq value t) 1.0)
     ((and (numberp value)
           (>= value 0))
      value)
     (t
      (error "packlet: :idle must evaluate to t, nil, or a non-negative number"))))

  (defun packlet--hook-function-form (function)
    "Return a function form for FUNCTION suitable for `add-hook'."
    `(function ,function))

  (defun packlet--key-form (key)
    "Return a form that resolves KEY for key binding APIs."
    (if (stringp key)
        `(kbd ,key)
      key)))

;;;###autoload
(defmacro packlet (feature &rest body)
  "Define lazy-first setup for FEATURE."
  (declare (indent 1))
  (unless (symbolp feature)
    (error "packlet: FEATURE must be a symbol"))
  (let* ((sections (packlet--parse-body body))
         (init-forms (packlet--section sections :init))
         (custom-forms (packlet--normalize-customs
                        (packlet--section sections :custom)))
         (config-forms (packlet--section sections :config))
         (commands (packlet--normalize-symbols
                    (packlet--section sections :commands)
                    :commands))
         (autoloads (packlet--normalize-symbols
                     (packlet--section sections :autoload)
                     :autoload))
         (modes (packlet--normalize-pairs
                 (packlet--section sections :mode)
                 :mode
                 #'packlet--mode-entry-p))
         (hooks (packlet--normalize-pairs
                 (packlet--section sections :hook)
                 :hook
                 #'packlet--hook-entry-p))
         (bindings (packlet--normalize-bindings
                    (packlet--section sections :bind)))
         (afters (packlet--normalize-symbols
                  (packlet--section sections :after)
                  :after))
         (idle-form (packlet--idle-form sections))
         (demand-form (packlet--demand-form sections))
         (functions (packlet--normalize-symbols
                     (packlet--section sections :functions)
                     :functions))
         (defines (packlet--normalize-symbols
                   (packlet--section sections :defines)
                   :defines))
         (file (packlet--file-form sections feature))
         (configured-var (intern (format "packlet--configured-%s" feature)))
         (config-function (intern (format "packlet--configure-%s" feature))))
    `(progn
       ,@(mapcar
          (lambda (variable)
            `(defvar ,variable))
          defines)
       ,@(mapcar
          (lambda (function)
            `(declare-function ,function ,file))
          functions)
       ,@init-forms
       ,@(mapcar
          (lambda (setting)
            `(setopt ,(car setting) ,(cadr setting)))
          custom-forms)
       ,@(mapcar
          (lambda (command)
            `(packlet--maybe-autoload ',command ,file t))
          commands)
       ,@(mapcar
          (lambda (function)
            `(packlet--maybe-autoload ',function ,file nil))
          autoloads)
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
            (pcase binding
              (`(:global ,key ,command)
               `(progn
                  (packlet--maybe-autoload ',command ,file t)
                  (global-set-key
                   ,(packlet--key-form key)
                   ',command)))
              (`(:map ,keymap ,key ,command)
               `(progn
                  (packlet--maybe-autoload ',command ,file t)
                  (packlet--register-keymap-binding
                   ',feature
                   ',keymap
                   ,(packlet--key-form key)
                  ',command
                   ',afters)))))
          bindings)
       ,@(when (packlet--has-section-p sections :idle)
           `((when-let ((delay (packlet--idle-delay ,idle-form)))
               (packlet--register-idle-load ',feature ',afters ,file delay))))
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
               (packlet--register-demand-load ',feature ',afters ,file)))))))

(provide 'packlet)

;;; packlet.el ends here
