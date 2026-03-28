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
(eval-and-compile
  (require 'macroexp))

(defgroup packlet nil
  "Lazy-first package setup DSL."
  :group 'lisp
  :prefix "packlet-")

(defcustom packlet-warn-on-missing-libraries t
  "Whether `packlet' should warn when an explicit library load fails."
  :type 'boolean
  :group 'packlet)

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

(defvar packlet--after-load-handlers (make-hash-table :test #'eq)
  "Registered after-load handlers keyed by watched feature.")

(defvar packlet--after-load-dispatchers (make-hash-table :test #'eq)
  "Features whose after-load dispatcher is already installed.")

(cl-defstruct (packlet--idle-state
               (:constructor packlet--make-idle-state))
  feature
  afters
  file
  delay
  startup-complete
  timer
  startup-hook)

(defvar packlet--idle-load-states (make-hash-table :test #'equal)
  "State for `:idle' loaders keyed by packlet expansion id.")

(defvar packlet--file-cleanups (make-hash-table :test #'equal)
  "Cleanup stacks for file-backed `packlet' evaluations.")

(defvar packlet--expansion-load-states (make-hash-table :test #'equal)
  "Expansion state for file-backed `packlet' macro calls.")

(defun packlet--warn (format-string &rest args)
  "Emit a `packlet' warning using FORMAT-STRING and ARGS."
  (when packlet-warn-on-missing-libraries
    (display-warning 'packlet
                     (apply #'format format-string args)
                     :warning)))

(defun packlet--register-file-cleanup (file cleanup)
  "Register CLEANUP to run before FILE is evaluated again."
  (when file
    (let ((file (expand-file-name file)))
      (puthash file
               (cons cleanup (gethash file packlet--file-cleanups))
               packlet--file-cleanups))))

(defun packlet--run-file-cleanups (file)
  "Run registered cleanup functions for FILE in reverse registration order."
  (when-let ((cleanups (gethash file packlet--file-cleanups)))
    (remhash file packlet--file-cleanups)
    (dolist (cleanup cleanups)
      (funcall cleanup))))

(defun packlet--prepare-file-eval (file)
  "Clear stale `packlet' registrations for FILE before reevaluation."
  (when file
    (let ((file (expand-file-name file)))
      (packlet--run-file-cleanups file)
      (remhash file packlet--expansion-load-states))))

(defun packlet--eval-buffer-source-file (buffer filename)
  "Return the source file used by `eval-buffer' for BUFFER and FILENAME."
  (or (and (stringp filename) filename)
      (when-let ((buffer (or (and (bufferp buffer) buffer)
                             (and buffer (get-buffer buffer))
                             (current-buffer))))
        (buffer-file-name buffer))))

(defun packlet--eval-buffer-advice (orig &optional buffer printflag filename
                                         unibyte do-allow-print)
  "Prepare file-backed `packlet' state before `eval-buffer'."
  ;; `load' evaluates source files via `eval-buffer'.  Skip cleanup in
  ;; that path so loading a feature file that happens to share the same
  ;; pathname as a previously evaluated config buffer does not wipe the
  ;; freshly registered handlers right before `provide'.
  (unless load-file-name
    (packlet--prepare-file-eval
     (packlet--eval-buffer-source-file buffer filename)))
  (funcall orig buffer printflag filename unibyte do-allow-print))

(defun packlet--load-file-advice (orig file &rest args)
  "Prepare file-backed `packlet' state before `load-file'."
  (packlet--prepare-file-eval file)
  (apply orig file args))

(unless (advice-member-p #'packlet--eval-buffer-advice 'eval-buffer)
  (advice-add 'eval-buffer :around #'packlet--eval-buffer-advice))

(unless (advice-member-p #'packlet--load-file-advice 'load-file)
  (advice-add 'load-file :around #'packlet--load-file-advice))

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

(defun packlet--load-library (file)
  "Load FILE and warn when it cannot be found."
  (or (load file t 'nomessage)
      (prog1 nil
        (packlet--warn "Could not load library `%s'" file))))

(defun packlet--load-feature (feature &optional file)
  "Load FEATURE, falling back to FILE when needed."
  (or (require feature nil t)
      (if file
          (packlet--load-library file)
        (prog1 nil
          (packlet--warn "Could not require feature `%s'" feature)))))

(defun packlet--after-load-handler-table (feature)
  "Return the handler table for watched FEATURE."
  (or (gethash feature packlet--after-load-handlers)
      (let ((table (make-hash-table :test #'equal)))
        (puthash feature table packlet--after-load-handlers)
        table)))

(defun packlet--unregister-after-load-handler (feature id)
  "Remove the handler registered under ID for FEATURE."
  (when-let ((table (gethash feature packlet--after-load-handlers)))
    (remhash id table)
    (when (= (hash-table-count table) 0)
      (remhash feature packlet--after-load-handlers))))

(defun packlet--run-after-load-handlers (feature)
  "Run registered handlers for watched FEATURE."
  (when-let ((table (gethash feature packlet--after-load-handlers)))
    (maphash
     (lambda (_id function)
       (funcall function))
     table)))

(defun packlet--register-after-load-handler (feature id function
                                                     &optional source-file)
  "Register FUNCTION under ID for watched FEATURE."
  (puthash id function (packlet--after-load-handler-table feature))
  (packlet--register-file-cleanup
   source-file
   (lambda ()
     (packlet--unregister-after-load-handler feature id)))
  (unless (gethash feature packlet--after-load-dispatchers)
    (puthash feature t packlet--after-load-dispatchers)
    (with-eval-after-load feature
      (packlet--run-after-load-handlers feature)))
  (when (featurep feature)
    (funcall function)))

(defun packlet--register-after-load (id feature function afters
                                        &optional source-file)
  "Run FUNCTION after FEATURE and AFTERS are loaded.
ID identifies the current `packlet' expansion."
  (packlet--register-after-load-handler
   feature (list id feature) function source-file)
  (dolist (after afters)
    (packlet--register-after-load-handler
     after (list id after) function source-file))
  (funcall function))

(defun packlet--register-demand-load (id feature afters &optional file source-file)
  "Load FEATURE once AFTERS are loaded.
When FILE is non-nil, use it as a fallback library name.
ID identifies the current `packlet' expansion."
  (let ((load-now
         (lambda ()
           (when (and (not (featurep feature))
                      (packlet--all-features-loaded-p afters))
             (packlet--load-feature feature file)))))
    (dolist (after afters)
      (packlet--register-after-load-handler
       after (list id after) load-now source-file))
    (funcall load-now)))

(defun packlet--idle-load-ready-p ()
  "Return non-nil when an idle load should proceed."
  (and (not (input-pending-p))
       (not (active-minibuffer-window))
       (not executing-kbd-macro)))

(defun packlet--cancel-idle-load-timer (state)
  "Cancel the active timer in idle loader STATE."
  (when (timerp (packlet--idle-state-timer state))
    (cancel-timer (packlet--idle-state-timer state)))
  (setf (packlet--idle-state-timer state) nil))

(defun packlet--idle-load-schedulable-p (state)
  "Return non-nil when idle loader STATE can schedule a load."
  (and (packlet--idle-state-startup-complete state)
       (not (featurep (packlet--idle-state-feature state)))
       (packlet--all-features-loaded-p (packlet--idle-state-afters state))))

(defun packlet--run-idle-load (id)
  "Run the idle loader registered under ID."
  (when-let ((state (gethash id packlet--idle-load-states)))
    (setf (packlet--idle-state-timer state) nil)
    (when (packlet--idle-load-schedulable-p state)
      (if (packlet--idle-load-ready-p)
          (packlet--load-feature (packlet--idle-state-feature state)
                                 (packlet--idle-state-file state))
        (packlet--maybe-schedule-idle-load id)))))

(defun packlet--maybe-schedule-idle-load (id)
  "Schedule the idle loader registered under ID when ready."
  (when-let ((state (gethash id packlet--idle-load-states)))
    (when (and (packlet--idle-load-schedulable-p state)
               (not (timerp (packlet--idle-state-timer state))))
      (setf (packlet--idle-state-timer state)
            (run-with-idle-timer
             (packlet--idle-state-delay state) nil
             #'packlet--run-idle-load
             id)))))

(defun packlet--register-idle-load (id feature afters
                                       &optional file delay source-file)
  "Load FEATURE after startup during idle time.
AFTERS must be loaded before scheduling begins.  When FILE is non-nil,
use it as a fallback library name.  DELAY is the idle duration in seconds.
ID identifies the current `packlet' expansion."
  (let ((state (or (gethash id packlet--idle-load-states)
                   (puthash id (packlet--make-idle-state)
                            packlet--idle-load-states))))
    (packlet--register-file-cleanup
     source-file
     (lambda ()
       (when-let ((cleanup-state (gethash id packlet--idle-load-states)))
         (packlet--cancel-idle-load-timer cleanup-state)
         (when-let ((startup-hook
                     (packlet--idle-state-startup-hook cleanup-state)))
           (remove-hook 'emacs-startup-hook startup-hook)
           (setf (packlet--idle-state-startup-hook cleanup-state) nil))
         (remhash id packlet--idle-load-states))))
    (packlet--cancel-idle-load-timer state)
    (when-let ((startup-hook (packlet--idle-state-startup-hook state)))
      (remove-hook 'emacs-startup-hook startup-hook)
      (setf (packlet--idle-state-startup-hook state) nil))
    (setf (packlet--idle-state-feature state) feature
          (packlet--idle-state-afters state) afters
          (packlet--idle-state-file state) file
          (packlet--idle-state-delay state) (or delay 1.0)
          (packlet--idle-state-startup-complete state) after-init-time)
    (unless (packlet--idle-state-startup-complete state)
      (let ((startup-hook
             (lambda ()
               (when-let ((idle-state (gethash id packlet--idle-load-states)))
                 (setf (packlet--idle-state-startup-complete idle-state) t)
                 (when-let ((registered-hook
                             (packlet--idle-state-startup-hook idle-state)))
                   (remove-hook 'emacs-startup-hook registered-hook)
                   (setf (packlet--idle-state-startup-hook idle-state) nil))
                 (packlet--maybe-schedule-idle-load id)))))
        (setf (packlet--idle-state-startup-hook state) startup-hook)
        (add-hook 'emacs-startup-hook startup-hook)))
    (dolist (after afters)
      (packlet--register-after-load-handler
       after
       (list id after)
       (lambda ()
         (packlet--maybe-schedule-idle-load id))
       source-file))
    (packlet--maybe-schedule-idle-load id)))

(defun packlet--resolve-keymap (keymap)
  "Return the keymap named by KEYMAP, or nil when it is not yet bound."
  (when (boundp keymap)
    (let ((value (symbol-value keymap)))
      (unless (keymapp value)
        (error "packlet: %S does not name a keymap" keymap))
      value)))

(defun packlet--register-keymap-binding (id feature keymap key command afters
                                            &optional source-file)
  "Bind KEY to COMMAND in KEYMAP when the map becomes available.
FEATURE and AFTERS are watched for opportunities to install the binding.
ID identifies the current `packlet' expansion."
  (let (installed-map previous-binding)
    (packlet--register-file-cleanup
     source-file
     (lambda ()
       (when installed-map
         (define-key installed-map key previous-binding)
         (setq installed-map nil
               previous-binding nil))))
    (let ((install
           (lambda ()
             (when-let ((map (packlet--resolve-keymap keymap)))
               (unless installed-map
                 (setq installed-map map
                       previous-binding (lookup-key map key)))
               (define-key map key command)))))
      (packlet--register-after-load-handler
       feature (list id feature) install source-file)
      (dolist (after afters)
        (packlet--register-after-load-handler
         after (list id after) install source-file))
      (funcall install))))

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

  (defvar packlet--expansion-load-states (make-hash-table :test #'equal)
    "Expansion state for file-backed `packlet' macro calls.")

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

;;;###autoload
(defmacro packlet (feature &rest body)
  "Define lazy-first setup for FEATURE."
  (declare (indent 1))
  (unless (symbolp feature)
    (error "packlet: FEATURE must be a symbol"))
  (let* ((sections (packlet--parse-body body))
         (site (packlet--expansion-site feature body))
         (source-file (packlet--site-file site))
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
                          feature
                          site
                          "config"))
         (config-function (packlet--generated-symbol
                           "packlet--configure"
                           feature
                           site
                           "config"))
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
            `(packlet--load-library ,lib))
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
            `(let ((existing (member ',mode auto-mode-alist)))
               (packlet--maybe-autoload ',(cdr mode) ,file t)
               (add-to-list 'auto-mode-alist ',mode)
               (unless existing
                 (packlet--register-file-cleanup
                  ,source-file
                  (lambda ()
                    (setq auto-mode-alist
                          (delete ',mode auto-mode-alist)))))))
          modes)
       ,@(cl-loop
          for hook in hooks
          for index from 0
          append
          (let* ((hook-var (car hook))
                 (function (if (packlet--delayed-hook-entry-p hook)
                               (cadr hook)
                             (cdr hook)))
                 (hook-function
                  (packlet--generated-symbol
                   "packlet--hook"
                   feature
                   site
                   (format "hook-%d" index))))
            (if (packlet--delayed-hook-entry-p hook)
                (let ((delay (caddr hook))
                      (timer-var
                       (packlet--generated-symbol
                        "packlet--delayed-hook-timer"
                        feature
                        site
                        (format "hook-%d" index))))
                  `((defvar ,timer-var nil)
                    ,@(when (symbolp function)
                        `((packlet--maybe-autoload ',function ,file nil)))
                    (when (timerp ,timer-var)
                      (cancel-timer ,timer-var)
                      (setq ,timer-var nil))
                    (defalias ',hook-function
                      (lambda ()
                        (when (timerp ,timer-var)
                          (cancel-timer ,timer-var))
                        (setq ,timer-var
                              (run-with-idle-timer
                               ,delay nil
                               (lambda ()
                                 (setq ,timer-var nil)
                                 (funcall ,(packlet--hook-function-form function)))))))
                    (add-hook ',hook-var ',hook-function)
                    (packlet--register-file-cleanup
                     ,source-file
                     (lambda ()
                       (remove-hook ',hook-var ',hook-function)
                       (when (boundp ',timer-var)
                         (when (timerp ,timer-var)
                           (cancel-timer ,timer-var))
                         (makunbound ',timer-var))
                       (when (fboundp ',hook-function)
                         (fmakunbound ',hook-function))))))
              `((progn
                  ,@(when (symbolp function)
                      `((packlet--maybe-autoload ',function ,file nil)))
                  (defalias ',hook-function
                    (lambda ()
                      (funcall ,(packlet--hook-function-form function))))
                  (add-hook ',hook-var ',hook-function)
                  (packlet--register-file-cleanup
                   ,source-file
                   (lambda ()
                     (remove-hook ',hook-var ',hook-function)
                     (when (fboundp ',hook-function)
                       (fmakunbound ',hook-function)))))))))
       ,@(cl-loop
          for binding in bindings
          for index from 0
          collect
          (let ((binding-id (list site :bind index)))
            (pcase binding
              (`(:global ,key ,command)
               `(let ((previous-binding
                       (lookup-key global-map ,(packlet--key-form key))))
                  (packlet--maybe-autoload ',command ,file t)
                  (global-set-key
                   ,(packlet--key-form key)
                   ',command)
                  (packlet--register-file-cleanup
                   ,source-file
                   (lambda ()
                     (define-key global-map
                                 ,(packlet--key-form key)
                                 previous-binding)))))
              (`(:map ,keymap ,key ,command)
               `(progn
                  (packlet--maybe-autoload ',command ,file t)
                  (packlet--register-keymap-binding
                   ',binding-id
                   ',feature
                   ',keymap
                   ,(packlet--key-form key)
                   ',command
                   ',afters
                   ,source-file))))))
       ,@(cl-loop
          for entry in after-loads
          for index from 0
          collect
          (let ((after-load-function
                 (packlet--generated-symbol
                  "packlet--after-load"
                  feature
                  site
                  (format "after-load-%d" index)))
                (after-load-id (list site :after-load index)))
            `(progn
               (defalias ',after-load-function
                 (lambda ()
                   ,@(cdr entry)))
               (packlet--register-after-load-handler
                ',(car entry)
                ',after-load-id
                ',after-load-function
                ,source-file)
               (packlet--register-file-cleanup
                ,source-file
                (lambda ()
                  (when (fboundp ',after-load-function)
                    (fmakunbound ',after-load-function)))))))
       ,@user-forms
       ,@(when (packlet--has-section-p sections :idle)
           `((when-let ((delay (packlet--idle-delay ,idle-form)))
               (packlet--register-idle-load
                ',(list site :idle)
                ',feature
                ',afters
                ,file
                delay
                ,source-file))))
       ,@(when config-forms
           `((defvar ,configured-var nil)
             (setq ,configured-var nil)
             (defalias ',config-function
               (lambda ()
                 (when (and (not ,configured-var)
                            (featurep ',feature)
                            (packlet--all-features-loaded-p ',afters))
                   (setq ,configured-var t)
                   ,@config-forms)))
             (packlet--register-after-load
              ',(list site :config)
              ',feature
              ',config-function
              ',afters
              ,source-file)
             (packlet--register-file-cleanup
              ,source-file
              (lambda ()
                (when (boundp ',configured-var)
                  (makunbound ',configured-var))
                (when (fboundp ',config-function)
                  (fmakunbound ',config-function))))))
       ,@(when (packlet--has-section-p sections :demand)
           `((when ,demand-form
               (packlet--register-demand-load
                ',(list site :demand)
                ',feature
                ',afters
                ,file
                ,source-file)))))))

(provide 'packlet)

;;; packlet.el ends here
