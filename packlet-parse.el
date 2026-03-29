;;; packlet-parse.el --- Parsing and normalization for packlet -*- lexical-binding: t; -*-

;; Copyright (C) 2026 SuzumiyaAoba
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Internal parsing, normalization, and expansion-site helpers for `packlet'.

;;; Code:

(require 'packlet-runtime)

(eval-and-compile
  (require 'macroexp)

  (defconst packlet--keywords
    '(:file :init :setq :custom :load :add-to-list :list :alist :config
            :commands :autoload :mode :remap :derived-mode :hook :bind
            :bind-keymap :prefix-map :enable :faces :advice :interpreter
            :magic :after :after-load :idle :magic-fallback :demand
            :functions :defines))

  (defvar packlet--user-keywords nil
    "Alist of (KEYWORD . PLIST) for user-defined keywords.
Each PLIST may contain:
  :normalize  Function taking (FORMS) and returning normalized forms.
              Called at macro-expansion time.
  :expand     Function taking (CONTEXT FORMS) and returning a list of
              Lisp forms to splice into the expansion.  CONTEXT is a
              plist with :feature, :file, :afters, and :sections.")

  (defun packlet--keyword-p (form)
    "Return non-nil when FORM is a supported `packlet' keyword."
    (or (memq form packlet--keywords)
        (assq form packlet--user-keywords)))

  (defun packlet--generated-load-list-entry-p (entry)
    "Return non-nil when ENTRY looks like a generated `packlet' helper."
    (let ((symbol
           (cond
            ((symbolp entry) entry)
            ((and (consp entry) (symbolp (cdr entry))) (cdr entry))
            (t nil))))
      (and symbol
           (string-prefix-p "packlet--" (symbol-name symbol)))))

  (defun packlet--initial-load-list-p (file load-list)
    "Return non-nil when LOAD-LIST looks like the first expansion for FILE."
    (and (member file load-list)
         (not (cl-some #'packlet--generated-load-list-entry-p load-list))))

  (defun packlet--current-buffer-tick-for-file (file)
    "Return the current buffer modification tick when it is visiting FILE."
    (when (and buffer-file-name
               (equal (expand-file-name buffer-file-name) file))
      (buffer-chars-modified-tick)))

  (defun packlet--next-load-expansion-index (file)
    "Return the next stable expansion index for FILE in the current load session."
    (let* ((state (gethash file packlet--expansion-load-states))
           (last-counter (plist-get state :counter))
           (last-point (plist-get state :point))
           (last-load-list (plist-get state :load-list))
           (last-buffer-tick (plist-get state :buffer-tick))
           (current-point (point))
           (current-buffer-tick (packlet--current-buffer-tick-for-file file))
           (reset
            (or (null state)
                (not (equal current-buffer-tick last-buffer-tick))
                (and last-point
                     (< current-point last-point))
                (and (packlet--initial-load-list-p file current-load-list)
                     (not (eq current-load-list last-load-list)))))
           (counter (if reset
                        1
                      (1+ (or last-counter 0)))))
      ;; The first expansion in a fresh `eval-buffer'/`load-file' pass
      ;; starts with a clean load list object, while later expansions in
      ;; the same pass already reference generated `packlet--...' helpers.
      ;; Combining that signal with point/tick tracking keeps repeated
      ;; evals stable without collapsing multiple packlets in one form.
      (puthash file
               (list :counter counter
                     :point current-point
                     :load-list current-load-list
                     :buffer-tick current-buffer-tick)
               packlet--expansion-load-states)
      counter))

  (defun packlet--expansion-site (feature body)
    "Return a stable site identifier for FEATURE with BODY."
    (let* ((file (or (macroexp-file-name)
                     buffer-file-name))
           (expanded-file (and file (expand-file-name file))))
      (or (and expanded-file
               (list 'load expanded-file
                     (packlet--next-load-expansion-index expanded-file)))
          (list 'eval
                (buffer-name (current-buffer))
                feature
                (sxhash-equal body)))))

  (defun packlet--site-file (site)
    "Return SITE's source file, or nil for non-file-backed sites."
    (and (eq (car-safe site) 'load)
         (cadr site)))

  (defun packlet--generated-symbol (prefix feature site &optional suffix)
    "Return an interned helper symbol for PREFIX, FEATURE, SITE, and SUFFIX."
    (let ((hash (logand most-positive-fixnum
                        (sxhash-equal (list prefix feature site suffix)))))
      (intern (format "%s-%s-%s-%x"
                      prefix
                      (symbol-name feature)
                      (or suffix "main")
                      hash))))

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

  (defun packlet--remap-entry-p (value)
    "Return non-nil when VALUE is a valid `:remap' entry."
    (and (consp value)
         (symbolp (car value))
         (symbolp (cdr value))))

  (defun packlet--derived-mode-entry-p (value)
    "Return non-nil when VALUE looks like a `:derived-mode' entry."
    (and (packlet--proper-list-p value)
         (symbolp (car-safe value))
         (symbolp (cadr value))
         (stringp (caddr value))))

  (defun packlet--face-entry-p (value)
    "Return non-nil when VALUE looks like a `:faces' entry."
    (and (packlet--proper-list-p value)
         (symbolp (car-safe value))
         (cdr value)))

  (defun packlet--magic-entry-p (value)
    "Return non-nil when VALUE is a valid `:magic' entry."
    (and (consp value)
         (or (stringp (car value))
             (symbolp (car value))
             (and (consp (car value))
                  (eq (caar value) 'lambda)))
         (or (symbolp (cdr value))
             (null (cdr value)))))

  (defun packlet--hook-entry-p (value)
    "Return non-nil when VALUE is a valid `:hook' entry."
    (and (consp value)
         (symbolp (car value))
         (let ((function (cdr value)))
           (or (symbolp function)
               (and (consp function)
                    (eq (car function) 'lambda))))))

  (defun packlet--hook-function-p (value)
    "Return non-nil when VALUE is a supported hook function form."
    (or (symbolp value)
        (and (consp value)
             (eq (car value) 'lambda))))

  (defun packlet--parse-keyword-options (options spec context)
    "Parse keyword OPTIONS according to SPEC, reporting errors for CONTEXT.
SPEC is an alist of (KEYWORD . VALIDATOR) where VALIDATOR is nil (accept any
value) or a predicate that returns non-nil for valid values.
CONTEXT is a string used in error messages.
Returns an alist of (KEYWORD . VALUE) for recognized keywords."
    (let (result)
      (while options
        (let ((key (pop options)))
          (unless (keywordp key)
            (error "packlet: invalid %s option %S" context key))
          (unless options
            (error "packlet: missing value for %s option %S" context key))
          (let* ((value (pop options))
                 (entry (assq key spec)))
            (unless entry
              (error "packlet: unsupported %s option %S" context key))
            (let ((validator (cdr entry)))
              (when (and validator
                         (not (funcall validator value)))
                (error "packlet: %S for :%s must satisfy %S, got %S"
                       key context validator value)))
            (push (cons key value) result))))
      (nreverse result)))

  (defun packlet--normalize-hook-options (options)
    "Normalize `:hook' OPTIONS into a plist."
    (let* ((bool-p (lambda (v) (memq v '(nil t))))
           (parsed (packlet--parse-keyword-options
                    options
                    `((:append . ,bool-p)
                      (:depth  . numberp)
                      (:local  . ,bool-p))
                    "hook"))
           (append (alist-get :append parsed))
           (depth  (alist-get :depth parsed)))
      (list :depth (or depth (if append 90 0))
            :local (alist-get :local parsed))))

  (defun packlet--normalize-hook-entry (value)
    "Normalize a single `:hook' VALUE into a plist."
    (cond
     ((packlet--hook-entry-p value)
      (list :hook (car value)
            :function (cdr value)
            :delay nil
            :depth 0
            :local nil))
     ((packlet--proper-list-p value)
      (let ((hook (car value))
            (rest (cdr value))
            delay)
        (unless (symbolp hook)
          (error "packlet: invalid entry %S for :hook" value))
        (unless rest
          (error "packlet: invalid entry %S for :hook" value))
        (unless (packlet--hook-function-p (car rest))
          (error "packlet: invalid hook function %S" (car rest)))
        (let ((function (car rest))
              (options (cdr rest)))
          (when (and options (numberp (car options)))
            (setq delay (pop options))
            (when (< delay 0)
              (error "packlet: invalid entry %S for :hook" value)))
          (append
           (list :hook hook
                 :function function
                 :delay delay)
           (packlet--normalize-hook-options options)))))
     (t
      (error "packlet: invalid entry %S for :hook" value))))

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

  (defun packlet--advice-how-p (value)
    "Return non-nil when VALUE is a supported advice HOW keyword."
    (memq value '(:around :before :after :override
                  :after-until :after-while
                  :before-until :before-while
                  :filter-args :filter-return)))

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

  (defun packlet--normalize-entries (forms keyword single-entry-p normalize-entry)
    "Normalize FORMS for KEYWORD into a flat list.
SINGLE-ENTRY-P is a predicate that returns non-nil when a form is a single
entry rather than a list of entries.  NORMALIZE-ENTRY is called on each
individual entry to produce its normalized form."
    (let (result)
      (dolist (form forms)
        (cond
         ((funcall single-entry-p form)
          (push (funcall normalize-entry form) result))
         ((packlet--proper-list-p form)
          (dolist (entry form)
            (push (funcall normalize-entry entry) result)))
         (t
          (error "packlet: invalid value %S for %S" form keyword))))
      (nreverse result)))

  (defun packlet--normalize-hooks (forms)
    "Normalize FORMS under `:hook' into a flat list of hook entries.
Each entry becomes a plist with :hook, :function, :delay, :depth, and :local."
    (packlet--normalize-entries
     forms :hook
     (lambda (f) (or (packlet--hook-entry-p f)
                     (and (packlet--proper-list-p f)
                          (symbolp (car-safe f)))))
     #'packlet--normalize-hook-entry))

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

  (defun packlet--normalize-list-options (options keyword)
    "Normalize KEYWORD OPTIONS into a plist."
    (let ((context (substring (symbol-name keyword) 1)))
      (let ((parsed (packlet--parse-keyword-options
                     options
                     '((:append  . nil)
                       (:compare . nil))
                     context)))
        (list :append  (alist-get :append parsed)
              :compare (alist-get :compare parsed)))))

  (defun packlet--normalize-list-entry (value keyword &optional default-compare)
    "Normalize a single KEYWORD VALUE into a plist.
When DEFAULT-COMPARE is non-nil, use it when VALUE does not provide
an explicit `:compare' option."
    (unless (packlet--proper-list-p value)
      (error "packlet: invalid entry %S for %S" value keyword))
    (let ((variable (car value))
          (rest (cdr value)))
      (unless (symbolp variable)
        (error "packlet: invalid entry %S for %S" value keyword))
      (unless rest
        (error "packlet: invalid entry %S for %S" value keyword))
      (let ((options
             (packlet--normalize-list-options (cdr rest) keyword)))
        (when (and default-compare
                   (null (plist-get options :compare)))
          (setq options
                (plist-put options :compare default-compare)))
        (append
         (list :kind keyword
               :variable variable
               :element (car rest))
         options))))

  (defun packlet--normalize-lists (forms keyword &optional default-compare)
    "Normalize FORMS under KEYWORD into a flat list of entries."
    (packlet--normalize-entries
     forms keyword
     (lambda (f) (and (packlet--proper-list-p f)
                      (symbolp (car-safe f))))
     (lambda (entry)
       (packlet--normalize-list-entry entry keyword default-compare))))

  (defun packlet--normalize-add-to-lists (forms)
    "Normalize FORMS under `:add-to-list' into a flat list of entries."
    (packlet--normalize-lists forms :add-to-list))

  (defun packlet--normalize-lists-alias (forms)
    "Normalize FORMS under `:list' into a flat list of entries."
    (packlet--normalize-lists forms :list))

  (defun packlet--normalize-alists (forms)
    "Normalize FORMS under `:alist' into a flat list of entries."
    (packlet--normalize-lists
     forms
     :alist
     '(function packlet--alist-entry-key-equal-p)))

  (defun packlet--normalize-enable-entry (value)
    "Normalize a single `:enable' VALUE into a plist."
    (cond
     ((symbolp value)
      (list :function value
            :arg 1))
     ((and (packlet--proper-list-p value)
           (symbolp (car-safe value))
           (consp (cdr value))
           (null (cddr value)))
      (list :function (car value)
            :arg (cadr value)))
     (t
      (error "packlet: invalid entry %S for :enable" value))))

  (defun packlet--normalize-enables (forms)
    "Normalize FORMS under `:enable' into a flat list of entries."
    (packlet--normalize-entries
     forms :enable
     (lambda (f)
       (or (symbolp f)
           (and (packlet--proper-list-p f)
                (symbolp (car-safe f)))))
     #'packlet--normalize-enable-entry))

  (defun packlet--normalize-face-entry (value)
    "Normalize a single `:faces' VALUE into a plist."
    (unless (packlet--proper-list-p value)
      (error "packlet: invalid entry %S for :faces" value))
    (let ((face (car value))
          (rest (cdr value))
          copy
          attributes)
      (unless (symbolp face)
        (error "packlet: invalid entry %S for :faces" value))
      (while rest
        (let ((attribute (pop rest)))
          (unless (keywordp attribute)
            (error "packlet: invalid face attribute %S in %S"
                   attribute value))
          (unless rest
            (error "packlet: missing value for face attribute %S in %S"
                   attribute value))
          (let ((attribute-value (pop rest)))
            (if (eq attribute :copy)
                (progn
                  (unless (symbolp attribute-value)
                    (error "packlet: invalid :copy source %S in %S"
                           attribute-value value))
                  (setq copy attribute-value))
              (setq attributes
                    (append attributes (list attribute attribute-value)))))))
      (unless (or copy attributes)
        (error "packlet: invalid entry %S for :faces" value))
      (list :face face
            :copy copy
            :attributes attributes)))

  (defun packlet--normalize-faces (forms)
    "Normalize FORMS under `:faces' into a flat list of entries."
    (packlet--normalize-entries
     forms :faces
     #'packlet--face-entry-p
     #'packlet--normalize-face-entry))

  (defun packlet--normalize-derived-mode-entry (value)
    "Normalize a single `:derived-mode' VALUE into a plist."
    (unless (packlet--derived-mode-entry-p value)
      (error "packlet: invalid entry %S for :derived-mode" value))
    (let ((mode (nth 0 value))
          (parent (nth 1 value))
          (name (nth 2 value))
          (rest (nthcdr 3 value))
          docstring)
      (when (stringp (car-safe rest))
        (setq docstring (car rest)
              rest (cdr rest)))
      (list :mode mode
            :parent parent
            :name name
            :docstring docstring
            :body rest)))

  (defun packlet--normalize-derived-modes (forms)
    "Normalize FORMS under `:derived-mode' into a flat list of entries."
    (packlet--normalize-entries
     forms :derived-mode
     #'packlet--derived-mode-entry-p
     #'packlet--normalize-derived-mode-entry))

  (defun packlet--normalize-advice-options (options)
    "Normalize `:advice' OPTIONS into a plist."
    (let ((parsed (packlet--parse-keyword-options
                   options
                   `((:name  . ,(lambda (v) (or (stringp v) (symbolp v))))
                     (:depth . numberp))
                   "advice")))
      (list :name  (alist-get :name parsed)
            :depth (alist-get :depth parsed))))

  (defun packlet--normalize-advice-entry (value)
    "Normalize a single `:advice' VALUE into a plist."
    (unless (packlet--proper-list-p value)
      (error "packlet: invalid entry %S for :advice" value))
    (let ((symbol (car value))
          (rest (cdr value)))
      (unless (symbolp symbol)
        (error "packlet: invalid entry %S for :advice" value))
      (unless (and (consp rest)
                   (packlet--advice-how-p (car rest)))
        (error "packlet: invalid advice position in %S" value))
      (unless (and (consp (cdr rest))
                   (packlet--hook-function-p (cadr rest)))
        (error "packlet: invalid advice function in %S" value))
      (append
       (list :symbol symbol
             :how (car rest)
             :function (cadr rest))
       (packlet--normalize-advice-options (cddr rest)))))

  (defun packlet--normalize-advices (forms)
    "Normalize FORMS under `:advice' into a flat list of entries."
    (packlet--normalize-entries
     forms :advice
     (lambda (f) (and (packlet--proper-list-p f)
                      (symbolp (car-safe f))))
     #'packlet--normalize-advice-entry))

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
      (dolist (section sections)
        (when-let ((entry (assq (car section) packlet--user-keywords)))
          (let* ((props (cdr entry))
                 (raw (cdr section))
                 (normalizer (plist-get props :normalize))
                 (expander (plist-get props :expand))
                 (normalized (if normalizer (funcall normalizer raw) raw))
                 (context (list :feature feature :file file
                                :afters afters :sections sections)))
            (when expander
              (setq forms (append forms (funcall expander context normalized)))))))
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

(provide 'packlet-parse)

;;; packlet-parse.el ends here
