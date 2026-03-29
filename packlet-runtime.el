;;; packlet-runtime.el --- Internal runtime support for packlet -*- lexical-binding: t; -*-

;; Copyright (C) 2026 SuzumiyaAoba
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Internal runtime helpers shared by the public `packlet' entry points.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

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
  "Registered after-load handlers keyed by watched feature.
Each value is a list of (ID . FUNCTION) entries in registration order.")

(defvar packlet--after-load-dispatchers (make-hash-table :test #'eq)
  "Dispatcher forms registered in `after-load-alist' by feature.")

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

(defvar packlet--expansion-load-states (make-hash-table :test #'equal)
  "Expansion state for file-backed `packlet' macro calls.")

(defvar packlet--site-features (make-hash-table :test #'equal)
  "Feature metadata keyed by packlet expansion site.")

(cl-defstruct (packlet--source-entry
               (:constructor packlet--make-source-entry))
  id
  install
  cleanup)

(defvar packlet--file-source-states (make-hash-table :test #'equal)
  "Registered file-backed `packlet' source entries keyed by file scope.")

(defvar packlet--buffer-source-states (make-hash-table :test #'eq :weakness 'key)
  "Registered buffer-backed `packlet' source entries keyed by buffer.")

(defvar packlet--active-source-scope nil
  "Dynamic source scope for the `packlet' evaluation session in progress.")

(defvar packlet--pending-source-entries nil
  "Entries registered during the active `packlet' evaluation session.")

(defun packlet--warn (format-string &rest args)
  "Emit a `packlet' warning using FORMAT-STRING and ARGS."
  (when packlet-warn-on-missing-libraries
    (display-warning 'packlet
                     (apply #'format format-string args)
                     :warning)))

(defun packlet--normalize-source-scope (scope)
  "Normalize SOURCE scope key SCOPE."
  (pcase scope
    (`(:file ,file)
     (list :file (expand-file-name file)))
    (`(:buffer ,buffer)
     (list :buffer buffer))
    (_ scope)))

(defun packlet--source-scope-file (file)
  "Return the source scope for FILE."
  (and file
       (packlet--normalize-source-scope (list :file file))))

(defun packlet--current-eval-scope (&optional buffer)
  "Return the current non-file-backed evaluation scope for BUFFER."
  (let ((buffer (or (and (bufferp buffer) buffer)
                    (and buffer (get-buffer buffer))
                    (current-buffer))))
    (when-let ((scope-buffer
                (and buffer
                     (if (minibufferp buffer)
                         (window-buffer (minibuffer-selected-window))
                       buffer))))
      (packlet--normalize-source-scope (list :buffer scope-buffer)))))

(defun packlet--buffer-source-scope (&optional buffer)
  "Return the preferred `packlet' source scope for BUFFER."
  (let ((buffer (or (and (bufferp buffer) buffer)
                    (and buffer (get-buffer buffer))
                    (current-buffer))))
    (or (and buffer
             (packlet--source-scope-file
              (buffer-file-name buffer)))
        (packlet--current-eval-scope buffer))))

(defun packlet--source-entries (scope)
  "Return the registered source entries for SCOPE."
  (let ((scope (packlet--normalize-source-scope scope)))
    (copy-sequence
     (pcase scope
       (`(:file ,_)
        (gethash scope packlet--file-source-states))
       (`(:buffer ,buffer)
        (gethash buffer packlet--buffer-source-states))
       (_ nil)))))

(defun packlet--set-source-entries (scope entries)
  "Set registered source ENTRIES for SCOPE."
  (let ((scope (packlet--normalize-source-scope scope)))
    (pcase scope
      (`(:file ,_)
       (if entries
           (puthash scope entries packlet--file-source-states)
         (remhash scope packlet--file-source-states)))
      (`(:buffer ,buffer)
       (if entries
           (puthash buffer entries packlet--buffer-source-states)
         (remhash buffer packlet--buffer-source-states))))))

(defun packlet--cleanup-source-scope (scope &optional preserve-failed)
  "Run cleanup for SOURCE SCOPE.
When PRESERVE-FAILED is non-nil, keep failed entries registered."
  (let* ((scope (packlet--normalize-source-scope scope))
         (entries (packlet--source-entries scope))
         (failed (packlet--run-source-entry-cleanups entries scope)))
    (packlet--set-source-entries scope (and preserve-failed failed))
    failed))

(defun packlet--merge-source-entry-pair (older newer)
  "Return a merged source entry from OLDER and NEWER.
The merged install reinstalls OLDER before NEWER, while cleanup runs
NEWER before OLDER."
  (packlet--make-source-entry
   :id (packlet--source-entry-id newer)
   :install (lambda ()
              (funcall (packlet--source-entry-install older))
              (funcall (packlet--source-entry-install newer)))
   :cleanup (lambda ()
              (funcall (packlet--source-entry-cleanup newer))
              (funcall (packlet--source-entry-cleanup older)))))

(defun packlet--merge-source-entry-lists (entries additions)
  "Merge ADDITIONS into ENTRIES, coalescing identical ids."
  (let ((result (copy-sequence entries)))
    (dolist (entry additions)
      (let* ((id (packlet--source-entry-id entry))
             (existing (cl-find id result
                                :key #'packlet--source-entry-id
                                :test #'equal)))
        (if existing
            (setq result
                  (packlet--replace-source-entry
                   result
                   (packlet--merge-source-entry-pair existing entry)))
          (setq result (append result (list entry))))))
    result))

(defun packlet--replace-source-entry (entries entry)
  "Return ENTRIES with ENTRY inserted or replaced by id."
  (let ((id (packlet--source-entry-id entry))
        (replaced nil)
        result)
    (dolist (existing entries)
      (if (equal (packlet--source-entry-id existing) id)
          (progn
            (push entry result)
            (setq replaced t))
        (push existing result)))
    (unless replaced
      (push entry result))
    (nreverse result)))

(defun packlet--run-source-entry-cleanups (entries scope)
  "Run cleanup functions in ENTRIES for SCOPE in reverse registration order.
Return entries whose cleanup failed."
  (let (failed)
    (dolist (entry (reverse (copy-sequence entries)))
      (condition-case err
          (funcall (packlet--source-entry-cleanup entry))
        (error
         (push entry failed)
         (packlet--warn "Cleanup for `%s' failed: %S" scope err))))
    (nreverse failed)))

(defun packlet--run-source-entry-installs (entries scope)
  "Run install functions in ENTRIES for SCOPE in registration order."
  (dolist (entry entries)
    (condition-case err
        (funcall (packlet--source-entry-install entry))
      (error
       (packlet--warn "Rollback for `%s' failed: %S" scope err)))))

(defun packlet--register-source-entry (scope id install cleanup)
  "Install and track a source-backed entry under ID for SCOPE."
  (let* ((scope (packlet--normalize-source-scope
                 (or scope packlet--active-source-scope)))
         (entry (packlet--make-source-entry
                 :id id
                 :install install
                 :cleanup cleanup)))
    (funcall install)
    (when scope
      (if (equal scope packlet--active-source-scope)
          (setq packlet--pending-source-entries
                (packlet--replace-source-entry packlet--pending-source-entries
                                              entry))
        (packlet--set-source-entries
         scope
         (packlet--replace-source-entry (packlet--source-entries scope)
                                        entry))))))

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

(defun packlet--after-load-handler-entries (feature)
  "Return ordered handler entries for watched FEATURE."
  (gethash feature packlet--after-load-handlers))

(defun packlet--register-after-load-dispatcher (feature)
  "Ensure FEATURE has a dispatcher registered in `after-load-alist'."
  (unless (gethash feature packlet--after-load-dispatchers)
    (let* ((existing (cdr (assq feature after-load-alist)))
           (existing-count (length existing)))
      (eval-after-load feature
        `(packlet--run-after-load-handlers ',feature))
      (when-let ((entry (assq feature after-load-alist))
                 (dispatcher (nth existing-count (cdr entry))))
        (puthash feature dispatcher packlet--after-load-dispatchers)))))

(defun packlet--unregister-after-load-dispatcher (feature)
  "Remove FEATURE's dispatcher form from `after-load-alist'."
  (when-let ((dispatcher-form (gethash feature packlet--after-load-dispatchers)))
    (if-let ((entry (assq feature after-load-alist)))
        (let ((remaining (delq dispatcher-form (cdr entry))))
          (if remaining
              (setcdr entry remaining)
            (setq after-load-alist
                  (assq-delete-all feature after-load-alist)))))
    (remhash feature packlet--after-load-dispatchers)))

(defun packlet--unregister-after-load-handler (feature id)
  "Remove the handler registered under ID for FEATURE."
  (when-let ((entries (packlet--after-load-handler-entries feature)))
    (setq entries (cl-remove id entries :key #'car :test #'equal))
    (if entries
        (puthash feature entries packlet--after-load-handlers)
      (remhash feature packlet--after-load-handlers)
      (packlet--unregister-after-load-dispatcher feature))))

(defun packlet--run-after-load-handlers (feature)
  "Run registered handlers for watched FEATURE."
  (dolist (entry (copy-sequence (packlet--after-load-handler-entries feature)))
    (funcall (cdr entry))))

(defun packlet--register-after-load-handler (feature id function
                                                     &optional source-file)
  "Register FUNCTION under ID for watched FEATURE."
  (packlet--register-source-entry
   (packlet--source-scope-file source-file)
   (list :after-load feature id)
   (lambda ()
     (let* ((entries (copy-sequence (packlet--after-load-handler-entries feature)))
            (cell (assoc id entries)))
       (if cell
           (setcdr cell function)
         (setq entries (append entries (list (cons id function)))))
       (puthash feature entries packlet--after-load-handlers))
     (packlet--register-after-load-dispatcher feature)
     (when (featurep feature)
       (funcall function)))
   (lambda ()
     (packlet--unregister-after-load-handler feature id))))

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
  (packlet--register-source-entry
   (packlet--source-scope-file source-file)
   (list :idle id)
   (lambda ()
     (let ((state (or (gethash id packlet--idle-load-states)
                      (puthash id (packlet--make-idle-state)
                               packlet--idle-load-states))))
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
   (lambda ()
     (when-let ((cleanup-state (gethash id packlet--idle-load-states)))
       (packlet--cancel-idle-load-timer cleanup-state)
       (when-let ((startup-hook
                   (packlet--idle-state-startup-hook cleanup-state)))
         (remove-hook 'emacs-startup-hook startup-hook)
         (setf (packlet--idle-state-startup-hook cleanup-state) nil))
       (remhash id packlet--idle-load-states)))))

(defun packlet--resolve-keymap (keymap)
  "Return the keymap named by KEYMAP, or nil when it is not yet bound."
  (when (boundp keymap)
    (let ((value (symbol-value keymap)))
      (unless (keymapp value)
        (error "packlet: %S does not name a keymap" keymap))
      value)))

(defun packlet--register-keymap-binding (id feature keymap key command afters
                                            &optional source-file
                                            restore-predicate)
  "Bind KEY to COMMAND in KEYMAP when the map becomes available.
FEATURE and AFTERS are watched for opportunities to install the binding.
ID identifies the current `packlet' expansion.

When RESTORE-PREDICATE is non-nil, it is called with the current binding
and the installed map during cleanup.  Returning non-nil allows cleanup to
restore the previous binding even when KEY no longer points at COMMAND."
  (let (installed-map previous-binding)
    (packlet--register-source-entry
     (packlet--source-scope-file source-file)
     (list :keymap id)
     (lambda ()
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
         (funcall install)))
     (lambda ()
       (when installed-map
         (let ((current (lookup-key installed-map key)))
           (when (or (eq current command)
                     (and restore-predicate
                          (funcall restore-predicate current installed-map)))
             (define-key installed-map key previous-binding)))
         (setq installed-map nil
               previous-binding nil))))))

(defun packlet--replay-command-keys ()
  "Replay the key sequence that invoked the current command."
  (setq unread-command-events
        (append (listify-key-sequence (this-command-keys-vector))
                unread-command-events)))

(defun packlet--unbind-variable (symbol)
  "Unbind SYMBOL if it is currently bound as a variable."
  (when (boundp symbol)
    (makunbound symbol)))

(defun packlet--unbind-function (symbol)
  "Remove SYMBOL's function definition if it has one."
  (when (fboundp symbol)
    (fmakunbound symbol)))

(defun packlet--list-member-p (element list &optional compare-fn)
  "Return non-nil when ELEMENT is present in LIST.
When COMPARE-FN is non-nil, use it as the equality predicate."
  (cl-member element list :test (or compare-fn #'equal)))

(defun packlet--remove-list-entry (element list &optional append compare-fn)
  "Remove one ELEMENT from LIST and return the new list.
When APPEND is non-nil, remove the last matching entry.  Otherwise remove
the first matching entry.  When COMPARE-FN is non-nil, use it for matching."
  (let* ((test (or compare-fn #'equal))
         (items (if append (reverse list) list))
         removed
         result)
    (dolist (item items)
      (if (and (not removed)
               (funcall test element item))
          (setq removed t)
        (push item result)))
    (if append
        result
      (nreverse result))))

(defun packlet--alist-entry-key-equal-p (left right)
  "Return non-nil when LEFT and RIGHT share the same alist key."
  (equal (car-safe left) (car-safe right)))

(defun packlet--face-snapshot (face)
  "Return a copy of FACE attributes."
  (copy-tree (face-all-attributes face nil)))

(defun packlet--restore-face-snapshot (face previous applied)
  "Restore PREVIOUS FACE attributes where current values still match APPLIED."
  (let (args)
    (dolist (entry previous)
      (let* ((attribute (car entry))
             (previous-value (cdr entry))
             (applied-value (alist-get attribute applied nil nil #'eq))
             (current-value (face-attribute face attribute nil 'default)))
        (when (equal current-value applied-value)
          (setq args (append args (list attribute previous-value))))))
    (when args
      (apply #'set-face-attribute face nil args))))

(defun packlet--capture-function-states (symbols)
  "Capture function binding state for SYMBOLS."
  (mapcar
   (lambda (symbol)
     (list symbol
           (fboundp symbol)
           (and (fboundp symbol)
                (symbol-function symbol))))
   symbols))

(defun packlet--restore-function-states (previous installed)
  "Restore PREVIOUS function states when current bindings still match INSTALLED."
  (dolist (entry previous)
    (pcase-let ((`(,symbol ,previous-bound ,previous-definition) entry))
      (when-let ((installed-entry (assq symbol installed)))
        (pcase-let ((`(,_ ,installed-bound ,installed-definition) installed-entry))
          (when (and (eq (fboundp symbol) installed-bound)
                     (or (not installed-bound)
                         (eq (symbol-function symbol) installed-definition)))
            (if previous-bound
                (fset symbol previous-definition)
              (packlet--unbind-function symbol))))))))

(defun packlet--capture-variable-states (symbols)
  "Capture variable binding state for SYMBOLS."
  (mapcar
   (lambda (symbol)
     (list symbol
           (boundp symbol)
           (and (boundp symbol)
                (symbol-value symbol))))
   symbols))

(defun packlet--restore-variable-states (previous installed)
  "Restore PREVIOUS variable states when current bindings still match INSTALLED."
  (dolist (entry previous)
    (pcase-let ((`(,symbol ,previous-bound ,previous-value) entry))
      (when-let ((installed-entry (assq symbol installed)))
        (pcase-let ((`(,_ ,installed-bound ,installed-value) installed-entry))
          (when (and (eq (boundp symbol) installed-bound)
                     (or (not installed-bound)
                         (equal (symbol-value symbol) installed-value)))
            (if previous-bound
                (set symbol previous-value)
              (packlet--unbind-variable symbol))))))))

(defun packlet--derived-mode-variable-symbols (mode)
  "Return auxiliary variable symbols generated for derived MODE."
  (mapcar
   (lambda (suffix)
     (intern (format "%s%s" mode suffix)))
   '("-hook" "-map" "-syntax-table" "-abbrev-table")))

(provide 'packlet-runtime)

;;; packlet-runtime.el ends here
