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

(defun packlet--current-autoload-file (function)
  "Return FUNCTION's current autoload file, or nil when unavailable."
  (when-let ((definition (and (fboundp function)
                              (symbol-function function))))
    (when (autoloadp definition)
      (nth 1 definition))))

(defun packlet--fallback-autoload-p (function file)
  "Return non-nil when FUNCTION currently autoloads from FILE."
  (equal (packlet--current-autoload-file function) file))

(defun packlet--maybe-autoload (function file &optional interactive)
  "Autoload FUNCTION from FILE when it is not already defined.
When INTERACTIVE is non-nil, register FUNCTION as an interactive command.

If FUNCTION is currently a fallback autoload from FILE, prefer package
autoloads when they become available later in the session."
  (when (symbolp function)
    (when (or (not (fboundp function))
              (packlet--fallback-autoload-p function file))
      (packlet--maybe-load-autoloads file)
      (when (or (not (fboundp function))
                (packlet--fallback-autoload-p function file))
        (autoload function file nil interactive)))))

(defvar packlet--loaded-autoloads (make-hash-table :test #'equal)
  "Autoload libraries that `packlet' has already loaded successfully.")

(defun packlet--autoloads-file (file)
  "Return the autoload library name for FILE."
  (format "%s-autoloads" file))

(defun packlet--maybe-load-autoloads (file)
  "Load FILE's autoload definitions once when available."
  (let ((autoloads-file (packlet--autoloads-file file)))
    (unless (gethash autoloads-file packlet--loaded-autoloads)
      (when (locate-library autoloads-file)
        (when (load autoloads-file nil t)
          (puthash autoloads-file t packlet--loaded-autoloads))))))

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
    '(:file :init :setq :custom :load :config :commands :autoload :mode :hook
            :bind :after :after-load :idle :demand :functions :defines))

  (defvar packlet--user-keywords nil
    "Alist of (KEYWORD . PLIST) for user-defined keywords.
Each PLIST may contain:
  :normalize  Function taking (FORMS) and returning normalized forms.
              Called at macro-expansion time.
  :expand     Function taking (CONTEXT FORMS) and returning a list of
              Lisp forms to splice into the expansion.  CONTEXT is a
              plist with :feature, :file, :afters, and :sections.")

  (defvar packlet--generated-symbol-counter 0
    "Counter for generating unique helper symbols during macro expansion.")

  (defun packlet--keyword-p (form)
    "Return non-nil when FORM is a supported `packlet' keyword."
    (or (memq form packlet--keywords)
        (assq form packlet--user-keywords)))

  (defun packlet--generated-symbol (prefix feature)
    "Return an interned helper symbol for PREFIX and FEATURE."
    (intern (format "%s-%s-%d"
                    prefix
                    (symbol-name feature)
                    (cl-incf packlet--generated-symbol-counter))))

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

  (defun packlet--delayed-hook-entry-p (value)
    "Return non-nil when VALUE is a delayed `:hook' entry (HOOK FUNCTION DELAY).
DELAY must be a non-negative number."
    (and (consp value)
         (symbolp (car value))
         (consp (cdr value))
         (let ((function (cadr value)))
           (or (symbolp function)
               (and (consp function)
                    (eq (car function) 'lambda))))
         (consp (cddr value))
         (numberp (caddr value))
         (>= (caddr value) 0)
         (null (cdddr value))))

  (defvar packlet--delayed-hook-counter 0
    "Counter for generating unique delayed-hook timer variables.")

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

  (defun packlet--normalize-hooks (forms)
    "Normalize FORMS under `:hook' into a flat list of hook entries.
Each entry is either (HOOK . FUNCTION) or (HOOK FUNCTION DELAY)."
    (let (result)
      (dolist (form forms)
        (cond
         ((packlet--hook-entry-p form)
          (push form result))
         ((packlet--delayed-hook-entry-p form)
          (push form result))
         ((packlet--proper-list-p form)
          (dolist (entry form)
            (cond
             ((packlet--hook-entry-p entry)
              (push entry result))
             ((packlet--delayed-hook-entry-p entry)
              (push entry result))
             (t
              (error "packlet: invalid entry %S for :hook" entry)))))
         (t
          (error "packlet: invalid value %S for :hook" form))))
      (nreverse result)))

  (defun packlet--autoload-entry-p (value)
    "Return non-nil when VALUE is a valid `:autoload' tuple entry.
A tuple entry is (FUNCTION FILE) or (FUNCTION FILE INTERACTIVE)."
    (and (consp value)
         (symbolp (car value))
         (stringp (cadr value))
         (or (null (cddr value))
             (and (consp (cddr value))
                  (null (cdddr value))))))

  (defun packlet--normalize-autoloads (forms)
    "Normalize FORMS under `:autoload' into a list of (FUNCTION FILE INTERACTIVE).
Each form may be a symbol, a list of symbols, a tuple (FUNCTION FILE),
a tuple (FUNCTION FILE INTERACTIVE), or a list of such entries."
    (let (result)
      (dolist (form forms)
        (cond
         ((null form))
         ((symbolp form)
          (push (list form nil nil) result))
         ((packlet--autoload-entry-p form)
          (push (list (car form) (cadr form) (caddr form)) result))
         ((packlet--proper-list-p form)
          (dolist (entry form)
            (cond
             ((symbolp entry)
              (push (list entry nil nil) result))
             ((packlet--autoload-entry-p entry)
              (push (list (car entry) (cadr entry) (caddr entry)) result))
             (t
              (error "packlet: invalid entry %S for :autoload" entry)))))
         (t
          (error "packlet: invalid value %S for :autoload" form))))
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

  (defun packlet--normalize-loads (forms)
    "Normalize FORMS under `:load' into a flat list of library name strings."
    (let (result)
      (dolist (form forms)
        (cond
         ((stringp form) (push form result))
         ((symbolp form) (push (symbol-name form) result))
         ((packlet--proper-list-p form)
          (dolist (entry form)
            (cond
             ((stringp entry) (push entry result))
             ((symbolp entry) (push (symbol-name entry) result))
             (t (error "packlet: :load entries must be strings or symbols, got %S" entry)))))
         (t (error "packlet: invalid value %S for :load" form))))
      (nreverse result)))

  (defun packlet--after-load-entry-p (value)
    "Return non-nil when VALUE is a valid `:after-load' entry.
An entry is (FEATURE BODY...) where FEATURE is a symbol."
    (and (consp value)
         (symbolp (car value))
         (cdr value)))

  (defun packlet--normalize-after-loads (forms)
    "Normalize FORMS under `:after-load' into a list of (FEATURE BODY...) entries."
    (let (result)
      (dolist (form forms)
        (cond
         ((packlet--after-load-entry-p form)
          (push form result))
         ((packlet--proper-list-p form)
          (dolist (entry form)
            (unless (packlet--after-load-entry-p entry)
              (error "packlet: :after-load entries must be (FEATURE BODY...), got %S" entry))
            (push entry result)))
         (t (error "packlet: invalid value %S for :after-load" form))))
      (nreverse result)))

  (defun packlet--hook-function-form (function)
    "Return a function form for FUNCTION suitable for `add-hook'."
    `(function ,function))

  (defun packlet--key-form (key)
    "Return a form that resolves KEY for key binding APIs."
    (if (stringp key)
        `(kbd ,key)
      key))

  (defun packlet--expand-user-keywords (sections feature file afters)
    "Expand user-defined keywords found in SECTIONS.
FEATURE, FILE, and AFTERS provide the packlet context."
    (let (forms)
      (dolist (entry packlet--user-keywords)
        (let* ((kw (car entry))
               (props (cdr entry))
               (raw (packlet--section sections kw)))
          (when raw
            (let* ((normalizer (plist-get props :normalize))
                   (expander (plist-get props :expand))
                   (normalized (if normalizer (funcall normalizer raw) raw))
                   (context (list :feature feature :file file
                                  :afters afters :sections sections)))
              (when expander
                (setq forms (append forms (funcall expander context normalized))))))))
      forms)))

;;;###autoload
(defun packlet-define-keyword (keyword &rest props)
  "Register KEYWORD as a user-defined packlet keyword.
PROPS is a plist with the following keys:

  :normalize  Function taking (FORMS) and returning normalized forms.
              Called at macro-expansion time.  Optional; when omitted,
              raw forms are passed through unchanged.

  :expand     Function taking (CONTEXT FORMS) and returning a list of
              Lisp forms to splice into the `packlet' expansion.
              CONTEXT is a plist with :feature, :file, :afters, and
              :sections.

Both functions run at macro-expansion time and must be defined in an
`eval-and-compile' block or loaded before any `packlet' form that
uses the keyword.

Example:

  (eval-and-compile
    (packlet-define-keyword :my-keyword
      :expand (lambda (ctx forms)
                (let ((feature (plist-get ctx :feature)))
                  (mapcar (lambda (f) \\=`(message \"%s: %S\" \\=',feature \\=',f))
                          forms)))))"
  (declare (indent 1))
  (unless (keywordp keyword)
    (error "packlet-define-keyword: KEYWORD must be a keyword symbol, got %S" keyword))
  (when (memq keyword packlet--keywords)
    (error "packlet-define-keyword: %S is a built-in keyword and cannot be redefined" keyword))
  (setf (alist-get keyword packlet--user-keywords) props))

;;;###autoload
(defmacro packlet (feature &rest body)
  "Define lazy-first setup for FEATURE."
  (declare (indent 1))
  (unless (symbolp feature)
    (error "packlet: FEATURE must be a symbol"))
  (let* ((sections (packlet--parse-body body))
         (init-forms (packlet--section sections :init))
         (setq-forms (packlet--normalize-customs
                      (packlet--section sections :setq)))
         (custom-forms (packlet--normalize-customs
                        (packlet--section sections :custom)))
         (load-libs (packlet--normalize-loads
                     (packlet--section sections :load)))
         (config-forms (packlet--section sections :config))
         (commands (packlet--normalize-symbols
                    (packlet--section sections :commands)
                    :commands))
         (autoloads (packlet--normalize-autoloads
                     (packlet--section sections :autoload)))
         (modes (packlet--normalize-pairs
                 (packlet--section sections :mode)
                 :mode
                 #'packlet--mode-entry-p))
         (hooks (packlet--normalize-hooks
                 (packlet--section sections :hook)))
         (bindings (packlet--normalize-bindings
                    (packlet--section sections :bind)))
         (afters (packlet--normalize-symbols
                  (packlet--section sections :after)
                  :after))
         (after-loads (packlet--normalize-after-loads
                       (packlet--section sections :after-load)))
         (idle-form (packlet--idle-form sections))
         (demand-form (packlet--demand-form sections))
         (functions (packlet--normalize-symbols
                     (packlet--section sections :functions)
                     :functions))
         (defines (packlet--normalize-symbols
                   (packlet--section sections :defines)
                   :defines))
         (file (packlet--file-form sections feature))
         (configured-var (packlet--generated-symbol
                          "packlet--configured"
                          feature))
         (config-function (packlet--generated-symbol
                           "packlet--configure"
                           feature))
         (user-forms (packlet--expand-user-keywords sections feature file afters)))
    `(progn
       ,@(mapcar
          (lambda (variable)
            `(defvar ,variable))
          defines)
       ,@(mapcar
          (lambda (function)
            `(declare-function ,function ,file))
          functions)
       ,@(mapcar
          (lambda (setting)
            `(setq ,(car setting) ,(cadr setting)))
          setq-forms)
       ,@(mapcar
          (lambda (setting)
            `(setopt ,(car setting) ,(cadr setting)))
          custom-forms)
       ,@(mapcar
          (lambda (lib)
            `(load ,lib t 'nomessage))
          load-libs)
       ,@init-forms
       ,@(mapcar
          (lambda (command)
            `(packlet--maybe-autoload ',command ,file t))
          commands)
       ,@(mapcar
          (lambda (entry)
            (let ((function (nth 0 entry))
                  (entry-file (nth 1 entry))
                  (interactive (nth 2 entry)))
              `(packlet--maybe-autoload ',function ,(or entry-file file) ,interactive)))
          autoloads)
       ,@(mapcar
          (lambda (mode)
            `(progn
               (packlet--maybe-autoload ',(cdr mode) ,file t)
               (add-to-list 'auto-mode-alist ',mode)))
          modes)
       ,@(cl-mapcan
          (lambda (hook)
            (if (packlet--delayed-hook-entry-p hook)
                (let* ((hook-var (car hook))
                       (function (cadr hook))
                       (delay (caddr hook))
                       (timer-var (intern (format "packlet--delayed-hook-timer-%s-%d"
                                                  feature
                                                  (cl-incf packlet--delayed-hook-counter)))))
                  `((defvar ,timer-var nil)
                    ,@(when (symbolp function)
                        `((packlet--maybe-autoload ',function ,file nil)))
                    (add-hook ',hook-var
                              (lambda ()
                                (when (timerp ,timer-var)
                                  (cancel-timer ,timer-var))
                                (setq ,timer-var
                                      (run-with-idle-timer
                                       ,delay nil
                                       (lambda ()
                                         (setq ,timer-var nil)
                                         (funcall ,(packlet--hook-function-form function)))))))))
              `((progn
                  ,(when (symbolp (cdr hook))
                     `(packlet--maybe-autoload ',(cdr hook) ,file nil))
                  (add-hook ',(car hook)
                            ,(packlet--hook-function-form (cdr hook)))))))
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
       ,@(mapcar
          (lambda (entry)
            `(with-eval-after-load ',(car entry)
               ,@(cdr entry)))
          after-loads)
       ,@user-forms
       ,@(when (packlet--has-section-p sections :idle)
           `((when-let ((delay (packlet--idle-delay ,idle-form)))
               (packlet--register-idle-load ',feature ',afters ,file delay))))
       ,@(when config-forms
           `((defvar ,configured-var nil)
             (defalias ',config-function
               (lambda ()
                 (when (and (not ,configured-var)
                            (featurep ',feature)
                            (packlet--all-features-loaded-p ',afters))
                   (setq ,configured-var t)
                   ,@config-forms)))
             (packlet--register-after-load
              ',feature
              ',config-function
              ',afters)))
       ,@(when (packlet--has-section-p sections :demand)
           `((when ,demand-form
               (packlet--register-demand-load ',feature ',afters ,file)))))))

(provide 'packlet)

;;; packlet.el ends here
