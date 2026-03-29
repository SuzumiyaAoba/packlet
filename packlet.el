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

(defun packlet--resolve-source-scope (source)
  "Normalize SOURCE into a registered `packlet' source scope."
  (let ((scope
         (cond
          ((null source)
           (packlet--buffer-source-scope))
          ((bufferp source)
           (packlet--buffer-source-scope source))
          ((stringp source)
           (packlet--source-scope-file source))
          (t
           (packlet--normalize-source-scope source)))))
    (unless scope
      (error "packlet: could not resolve source scope from %S" source))
    scope))

(defun packlet--source-scope-name (scope)
  "Return a human-readable label for SOURCE SCOPE."
  (pcase (packlet--normalize-source-scope scope)
    (`(:file ,file)
     (format "File: %s" file))
    (`(:buffer ,buffer)
     (format "Buffer: %s" (buffer-name buffer)))
    (_
     (format "Scope: %S" scope))))

(defun packlet--source-entry-kind (entry)
  "Return a short kind label for source ENTRY."
  (let ((id (packlet--source-entry-id entry)))
    (pcase id
      (`(:site-feature ,_ ,_)
       :site-feature)
      (`(:after-load ,_ ,_)
       :after-load-handler)
      (`(:idle ,_)
       :idle-loader)
      (`(:keymap ,_)
       :keymap-binding)
      (`(,_ ,kind ,_)
       (if (keywordp kind)
           kind
         :entry))
      (_
       :entry))))

(defun packlet--source-entry-visible-p (entry)
  "Return non-nil when source ENTRY should be shown in user-facing output."
  (not (eq (packlet--source-entry-kind entry) :site-feature)))

(defun packlet--display-source-entries (entries)
  "Return user-visible ENTRIES from source ENTRIES."
  (cl-remove-if-not #'packlet--source-entry-visible-p entries))

(defun packlet--entry-site-p (value)
  "Return non-nil when VALUE looks like a packlet expansion site."
  (pcase value
    (`(load ,_ ,_) t)
    (`(eval ,_ ,_ ,_) t)
    (_ nil)))

(defun packlet--find-entry-site (value)
  "Return the expansion site nested inside VALUE, or nil when absent."
  (cond
   ((packlet--entry-site-p value)
    value)
   ((consp value)
    (or (packlet--find-entry-site (car value))
        (packlet--find-entry-site (cdr value))))
   ((vectorp value)
    (let ((index 0)
          site)
      (while (and (< index (length value))
                  (not site))
        (setq site (packlet--find-entry-site (aref value index))
              index (1+ index)))
      site))
   (t nil)))

(defun packlet--site-feature (site)
  "Return the feature associated with expansion SITE."
  (or (plist-get (gethash site packlet--site-features) :feature)
      (pcase site
        (`(eval ,_ ,feature ,_)
         feature)
        (_
         nil))))

(defun packlet--site-metadata-put (site property value)
  "Set PROPERTY to VALUE for expansion SITE metadata."
  (when-let ((metadata (copy-sequence (gethash site packlet--site-features))))
    (puthash site (plist-put metadata property value)
             packlet--site-features)))

(defun packlet--source-entry-feature (entry)
  "Return the configured feature associated with source ENTRY."
  (when-let ((site (packlet--find-entry-site (packlet--source-entry-id entry))))
    (packlet--site-feature site)))

(defun packlet--all-source-groups ()
  "Return all registered source groups as (SCOPE . ENTRIES)."
  (let (groups)
    (maphash
     (lambda (scope entries)
       (push (cons scope (copy-sequence entries)) groups))
     packlet--file-source-states)
    (maphash
     (lambda (buffer entries)
       (push (cons (list :buffer buffer) (copy-sequence entries)) groups))
     packlet--buffer-source-states)
    (sort groups
          (lambda (left right)
            (string-lessp (packlet--source-scope-name (car left))
                          (packlet--source-scope-name (car right)))))))

(defun packlet--feature-source-groups (feature)
  "Return source groups that register FEATURE."
  (let (groups)
    (dolist (group (packlet--all-source-groups))
      (let ((entries
             (cl-remove-if-not
              (lambda (entry)
                (and (packlet--source-entry-visible-p entry)
                     (eq (packlet--source-entry-feature entry) feature)))
              (cdr group))))
        (when entries
          (push (cons (car group) entries) groups))))
    (nreverse groups)))

(defun packlet--describe-source-string (source)
  "Return a textual description of `packlet' SOURCE registrations."
  (let* ((scope (packlet--resolve-source-scope source))
         (entries (packlet--display-source-entries
                   (packlet--source-entries scope))))
    (concat
     (packlet--source-scope-name scope)
     "\n"
     (format "Entries: %d\n" (length entries))
     (if entries
         (mapconcat
          (lambda (entry)
            (format "- %s %S"
                    (packlet--source-entry-kind entry)
                    (packlet--source-entry-id entry)))
          entries
          "\n")
       "- none"))))

;;;###autoload
(defun packlet-describe-source (&optional source)
  "Describe the `packlet' registrations owned by SOURCE.
SOURCE may be nil, a file name, a buffer, or a normalized source scope."
  (interactive)
  (let ((description (packlet--describe-source-string source)))
    (when (called-interactively-p 'interactive)
      (with-help-window (help-buffer)
       (princ description)))
    description))

(defun packlet--registered-features ()
  "Return registered packlet features sorted by name."
  (sort
   (delete-dups
    (let (result)
      (maphash
       (lambda (_site metadata)
         (when-let ((feature (plist-get metadata :feature)))
           (push feature result)))
       packlet--site-features)
      result))
   (lambda (left right)
     (string-lessp (symbol-name left)
                   (symbol-name right)))))

(defun packlet--read-feature-name ()
  "Read a feature name for interactive packlet commands."
  (let* ((registered
          (delete-dups
           (append
            (mapcar #'symbol-name
                    (packlet--registered-features))
            (mapcar #'symbol-name features))))
         (default (car registered)))
    (intern
     (completing-read
      (if default
          (format "Feature (default %s): " default)
        "Feature: ")
      registered
      nil
      nil
      nil
      nil
      default))))

(defun packlet--list-features-string ()
  "Return a textual listing of registered `packlet' features."
  (let ((features (packlet--registered-features)))
    (concat
     (format "Features: %d\n" (length features))
     (if features
         (mapconcat
          (lambda (feature)
            (format "- %s loaded=%s sources=%d"
                    feature
                    (if (featurep feature) "yes" "no")
                    (length (packlet--feature-source-groups feature))))
          features
          "\n")
       "- none"))))

;;;###autoload
(defun packlet-list-features ()
  "List registered `packlet' features."
  (interactive)
  (let ((description (packlet--list-features-string)))
    (when (called-interactively-p 'interactive)
      (with-help-window (help-buffer)
        (princ description)))
    description))

(defun packlet--describe-feature-string (feature)
  "Return a textual description of `packlet' FEATURE registrations."
  (let* ((groups (packlet--feature-source-groups feature))
         (sections
          (mapcar
           (lambda (group)
             (concat
              (packlet--source-scope-name (car group))
              "\n"
              (format "Entries: %d\n" (length (cdr group)))
              (mapconcat
               (lambda (entry)
                 (format "- %s %S"
                         (packlet--source-entry-kind entry)
                         (packlet--source-entry-id entry)))
               (cdr group)
               "\n")))
           groups)))
    (concat
     (format "Feature: %s\n" feature)
     (format "Loaded: %s\n" (if (featurep feature) "yes" "no"))
     (format "Sources: %d\n" (length groups))
     (if sections
         (concat "\n" (mapconcat #'identity sections "\n\n"))
       "- none"))))

(defun packlet--site-entries (site entries)
  "Return ENTRIES associated with expansion SITE."
  (cl-remove-if-not
   (lambda (entry)
     (equal (packlet--find-entry-site (packlet--source-entry-id entry))
            site))
   entries))

(defun packlet--site-has-entry-p (site kind entries)
  "Return non-nil when SITE has a KIND entry inside ENTRIES."
  (cl-some
   (lambda (entry)
     (let ((id (packlet--source-entry-id entry)))
       (and (consp id)
            (equal (car id) site)
            (eq (cadr id) kind))))
   entries))

(defun packlet--site-missing-afters (metadata)
  "Return unmet `:after' dependencies from METADATA."
  (cl-remove-if #'featurep (copy-sequence (plist-get metadata :afters))))

(defun packlet--idle-status (feature site metadata _entries)
  "Return the idle-load status for FEATURE at SITE from METADATA."
  (let ((state (gethash (list site :idle) packlet--idle-load-states)))
    (cond
     ((not (plist-get metadata :has-idle))
      "none")
     ((featurep feature)
      "loaded")
     ((not state)
      "disabled")
     ((not (packlet--all-features-loaded-p (plist-get metadata :afters)))
      "waiting for afters")
     ((not (packlet--idle-state-startup-complete state))
      "waiting for startup")
     ((timerp (packlet--idle-state-timer state))
      "scheduled")
     (t
      "ready"))))

(defun packlet--demand-status (feature _site metadata _entries)
  "Return the demand-load status for FEATURE from METADATA."
  (cond
   ((not (plist-get metadata :has-demand))
    "none")
   ((featurep feature)
    "loaded")
   ((not (plist-get metadata :demand-enabled))
    "disabled")
   ((packlet--all-features-loaded-p (plist-get metadata :afters))
    "ready")
   (t
    "waiting for afters")))

(defun packlet--config-status (feature metadata)
  "Return the config status for FEATURE from METADATA."
  (let ((configured-var (plist-get metadata :configured-var)))
    (cond
     ((not (plist-get metadata :has-config))
      "none")
     ((and configured-var
           (boundp configured-var)
           (symbol-value configured-var))
      "done")
     ((not (featurep feature))
      "waiting for feature")
     ((packlet--site-missing-afters metadata)
      "waiting for afters")
     (t
      "ready"))))

(defun packlet--feature-explanations (feature)
  "Return explanation groups for FEATURE."
  (let (groups)
    (dolist (group (packlet--all-source-groups))
      (let ((scope (car group))
            (entries (cdr group))
            sites)
        (dolist (entry entries)
          (when (eq (packlet--source-entry-feature entry) feature)
            (when-let ((site (packlet--find-entry-site
                              (packlet--source-entry-id entry))))
              (cl-pushnew site sites :test #'equal))))
        (dolist (site (nreverse sites))
          (when-let ((metadata (gethash site packlet--site-features)))
            (push (list :scope scope
                        :site site
                        :entries (packlet--site-entries site entries)
                        :metadata metadata)
                  groups)))))
    (nreverse groups)))

(defun packlet--explain-feature-string (feature)
  "Return an explanation of `packlet' FEATURE state."
  (let* ((groups (packlet--feature-explanations feature))
         (sections
          (mapcar
           (lambda (group)
             (let* ((scope (plist-get group :scope))
                    (entries (packlet--display-source-entries
                              (plist-get group :entries)))
                    (metadata (plist-get group :metadata))
                    (missing-afters (packlet--site-missing-afters metadata)))
               (mapconcat
                #'identity
                (delq
                 nil
                 (list
                  (packlet--source-scope-name scope)
                  (format "Entries: %d" (length entries))
                  (format "Afters: %s"
                          (if-let ((afters (plist-get metadata :afters)))
                              (mapconcat #'symbol-name afters ", ")
                            "none"))
                  (format "Missing afters: %s"
                          (if missing-afters
                              (mapconcat #'symbol-name missing-afters ", ")
                            "none"))
                  (format "Config: %s"
                          (packlet--config-status feature metadata))
                  (format "Demand: %s"
                          (packlet--demand-status feature
                                                  (plist-get group :site)
                                                  metadata
                                                  (plist-get group :entries)))
                  (format "Idle: %s"
                          (packlet--idle-status feature
                                                (plist-get group :site)
                                                metadata
                                                (plist-get group :entries)))))
                "\n")))
           groups)))
    (concat
     (format "Feature: %s\n" feature)
     (format "Loaded: %s\n" (if (featurep feature) "yes" "no"))
     (format "Sources: %d\n" (length groups))
     (if sections
         (concat "\n" (mapconcat #'identity sections "\n\n"))
       "- none"))))

;;;###autoload
(defun packlet-describe-feature (feature)
  "Describe the `packlet' registrations associated with FEATURE."
  (interactive (list (packlet--read-feature-name)))
  (unless (symbolp feature)
    (error "packlet: FEATURE must be a symbol, got %S" feature))
  (let ((description (packlet--describe-feature-string feature)))
    (when (called-interactively-p 'interactive)
      (with-help-window (help-buffer)
        (princ description)))
    description))

;;;###autoload
(defun packlet-explain-feature (feature)
  "Explain the current `packlet' runtime state for FEATURE."
  (interactive (list (packlet--read-feature-name)))
  (unless (symbolp feature)
    (error "packlet: FEATURE must be a symbol, got %S" feature))
  (let ((description (packlet--explain-feature-string feature)))
    (when (called-interactively-p 'interactive)
      (with-help-window (help-buffer)
        (princ description)))
    description))

;;;###autoload
(defun packlet-cleanup-source (&optional source)
  "Clean up the `packlet' registrations owned by SOURCE.
SOURCE may be nil, a file name, a buffer, or a normalized source scope.
Return entries whose cleanup failed."
  (interactive)
  (let* ((scope (packlet--resolve-source-scope source))
         (failed (packlet--cleanup-source-scope scope t)))
    (when (called-interactively-p 'interactive)
      (message "packlet cleaned %s%s"
               (packlet--source-scope-name scope)
               (if failed
                   (format " (%d cleanup failures preserved)"
                           (length failed))
                 "")))
    failed))

(defun packlet--form-contains-packlet-p (form &optional seen)
  "Return non-nil when FORM appears to contain a `packlet' call."
  (let ((seen (or seen (make-hash-table :test #'eq))))
    (cond
     ((symbolp form) (eq form 'packlet))
     ((or (not form)
          (numberp form)
          (stringp form)
          (keywordp form))
      nil)
     ((gethash form seen) nil)
     ((consp form)
      (puthash form t seen)
      (or (eq (car form) 'packlet)
          (packlet--form-contains-packlet-p (car form) seen)
          (packlet--form-contains-packlet-p (cdr form) seen)))
     ((or (vectorp form) (recordp form))
      (puthash form t seen)
      (let ((index 0)
            found)
        (while (and (< index (length form))
                    (not found))
          (setq found
                (packlet--form-contains-packlet-p
                 (aref form index)
                 seen)
                index (1+ index)))
        found))
     (t nil))))

(defun packlet--with-source-session (scope expansion-file thunk)
  "Evaluate THUNK transactionally for SOURCE SCOPE.
When EXPANSION-FILE is non-nil, reset its expansion counter for the session."
  (let ((scope (packlet--normalize-source-scope scope)))
    (if (or (null scope)
            packlet--active-source-scope)
        (funcall thunk)
      (let* ((old-entries (packlet--source-entries scope))
             (old-expansion-state
              (and expansion-file
                   (gethash expansion-file packlet--expansion-load-states)))
             (old-cleanup-failures
              (packlet--cleanup-source-scope scope t)))
        (when expansion-file
          (remhash expansion-file packlet--expansion-load-states))
        (let ((packlet--active-source-scope scope)
              (packlet--pending-source-entries nil))
          (condition-case err
              (let ((result (funcall thunk)))
                (packlet--set-source-entries
                 scope
                 (packlet--merge-source-entry-lists
                  old-cleanup-failures
                  packlet--pending-source-entries))
                result)
            (error
             (packlet--run-source-entry-cleanups
              packlet--pending-source-entries
              scope)
             (packlet--run-source-entry-installs old-entries scope)
             (packlet--set-source-entries
              scope
              (append old-entries old-cleanup-failures))
             (when expansion-file
               (if old-expansion-state
                   (puthash expansion-file
                            old-expansion-state
                            packlet--expansion-load-states)
                 (remhash expansion-file packlet--expansion-load-states)))
             (signal (car err) (cdr err)))))))))

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
  (if load-file-name
      (funcall orig buffer printflag filename unibyte do-allow-print)
    (let* ((file (packlet--eval-buffer-source-file buffer filename))
           (scope (or (packlet--source-scope-file file)
                      (packlet--current-eval-scope buffer))))
      (packlet--with-source-session
       scope
       (and file (expand-file-name file))
       (lambda ()
         (funcall orig buffer printflag filename unibyte do-allow-print))))))

(defun packlet--load-file-advice (orig file &rest args)
  "Prepare file-backed `packlet' state before `load-file'."
  (let ((file (expand-file-name file)))
    (packlet--with-source-session
     (packlet--source-scope-file file)
     file
     (lambda ()
       (apply orig file args)))))

(defun packlet--eval-advice (orig form &optional lexical)
  "Prepare non-file-backed `packlet' state before direct `eval'."
  (let ((lisp-buffer-p
         (derived-mode-p 'emacs-lisp-mode 'lisp-interaction-mode)))
    (if (or load-file-name
            packlet--active-source-scope
            (not (if lisp-buffer-p
                     (packlet--form-contains-packlet-p form)
                   (eq (car-safe form) 'packlet))))
      (funcall orig form lexical)
      (packlet--with-source-session
       (packlet--current-eval-scope)
       nil
       (lambda ()
         (funcall orig form lexical))))))

(defun packlet--kill-buffer-hook ()
  "Clean up `packlet' registrations owned by the current buffer."
  (packlet--cleanup-source-scope
   (packlet--normalize-source-scope (list :buffer (current-buffer)))))

(unless (advice-member-p #'packlet--eval-buffer-advice 'eval-buffer)
  (advice-add 'eval-buffer :around #'packlet--eval-buffer-advice))

(unless (advice-member-p #'packlet--load-file-advice 'load-file)
  (advice-add 'load-file :around #'packlet--load-file-advice))

(unless (advice-member-p #'packlet--eval-advice 'eval)
  (advice-add 'eval :around #'packlet--eval-advice))

(unless (member #'packlet--kill-buffer-hook kill-buffer-hook)
  (add-hook 'kill-buffer-hook #'packlet--kill-buffer-hook))

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

(eval-and-compile
  (defconst packlet--keywords
    '(:file :init :setq :custom :load :add-to-list :list :alist :config
            :commands :autoload :mode :hook :bind :bind-keymap :prefix-map
            :enable :advice :interpreter :magic :after :after-load :idle
            :magic-fallback :demand :functions :defines))

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

  (defun packlet--expand-settings (kind feature site source-scope settings)
    "Expand SETTINGS for KIND in FEATURE at SITE under SOURCE-SCOPE."
    (cl-loop
     for setting in settings
     for index from 0
     append
     (let* ((variable (car setting))
            (value-form (cadr setting))
            (value-var
             (packlet--generated-symbol
              "packlet--setting-value"
              feature
              site
              (format "%s-%d" (substring (symbol-name kind) 1) index)))
            (had-value-var
             (packlet--generated-symbol
              "packlet--setting-bound"
              feature
              site
              (format "%s-%d" (substring (symbol-name kind) 1) index)))
            (previous-value-var
             (packlet--generated-symbol
              "packlet--setting-previous"
              feature
              site
              (format "%s-%d" (substring (symbol-name kind) 1) index)))
            (install-form
             (pcase kind
               (:setq `(setq ,variable ,value-var))
               (:custom `(setopt ,variable ,value-var))
               (_ (error "packlet: unsupported setting kind %S" kind))))
            (restore-form
             (pcase kind
               (:setq `(setq ,variable ,previous-value-var))
               (:custom `(setopt ,variable ,previous-value-var))
               (_ (error "packlet: unsupported setting kind %S" kind)))))
       `((defvar ,value-var nil)
         (defvar ,had-value-var nil)
         (defvar ,previous-value-var nil)
         (packlet--register-source-entry
          ',source-scope
          ',(list site kind index)
          (lambda ()
            (setq ,value-var ,value-form
                  ,had-value-var (boundp ',variable))
            (if ,had-value-var
                (setq ,previous-value-var ,variable)
              (packlet--unbind-variable ',previous-value-var))
            ,install-form)
          (lambda ()
            (when (and (boundp ',variable)
                       (equal ,variable ,value-var))
              (if ,had-value-var
                  ,restore-form
                (makunbound ',variable)))
            (packlet--unbind-variable ',value-var)
            (packlet--unbind-variable ',had-value-var)
            (packlet--unbind-variable ',previous-value-var)))))))

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
         (source-scope (and source-file
                            (list :file source-file)))
         (init-forms (packlet--section sections :init))
         (setq-forms (packlet--normalize-customs
                      (packlet--section sections :setq)))
         (custom-forms (packlet--normalize-customs
                        (packlet--section sections :custom)))
         (load-libs (packlet--normalize-loads
                     (packlet--section sections :load)))
         (add-to-lists (append
                        (packlet--normalize-add-to-lists
                         (packlet--section sections :add-to-list))
                        (packlet--normalize-lists-alias
                         (packlet--section sections :list))
                        (packlet--normalize-alists
                         (packlet--section sections :alist))))
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
         (interpreters (packlet--normalize-pairs
                        (packlet--section sections :interpreter)
                        :interpreter
                        #'packlet--mode-entry-p))
         (magics (packlet--normalize-pairs
                  (packlet--section sections :magic)
                  :magic
                  #'packlet--magic-entry-p))
         (magic-fallbacks (packlet--normalize-pairs
                           (packlet--section sections :magic-fallback)
                           :magic-fallback
                           #'packlet--magic-entry-p))
         (hooks (packlet--normalize-hooks
                 (packlet--section sections :hook)))
         (bindings (packlet--normalize-bindings
                    (packlet--section sections :bind)))
         (keymap-bindings (packlet--normalize-bindings
                           (packlet--section sections :bind-keymap)))
         (prefix-maps (packlet--normalize-symbols
                       (packlet--section sections :prefix-map)
                       :prefix-map))
         (enables (packlet--normalize-enables
                   (packlet--section sections :enable)))
         (advices (packlet--normalize-advices
                   (packlet--section sections :advice)))
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
       (packlet--register-source-entry
        ',source-scope
        ',(list :site-feature site feature)
        (lambda ()
          (puthash ',site
                   (list :feature ',feature
                         :file ,file
                         :afters ',afters
                         :has-config ,(and config-forms t)
                         :configured-var ',(and config-forms configured-var)
                         :has-demand ,(and (packlet--has-section-p sections :demand) t)
                         :demand-enabled nil
                         :has-idle ,(and (packlet--has-section-p sections :idle) t))
                   packlet--site-features))
        (lambda ()
          (remhash ',site packlet--site-features)))
       ,@(packlet--expand-settings :setq feature site source-scope setq-forms)
       ,@(packlet--expand-settings :custom feature site source-scope custom-forms)
       ,@(mapcar
          (lambda (lib)
            `(packlet--load-library ,lib))
          load-libs)
       ,@init-forms
       ,@(cl-loop
          for entry in add-to-lists
          for index from 0
          append
          (let* ((variable (plist-get entry :variable))
                 (kind (plist-get entry :kind))
                 (element (plist-get entry :element))
                 (append-form (plist-get entry :append))
                 (compare-form (plist-get entry :compare))
                 (value-var
                  (packlet--generated-symbol
                   "packlet--list-value"
                   feature
                   site
                   (format "add-to-list-%d" index)))
                 (append-var
                  (packlet--generated-symbol
                   "packlet--list-append"
                   feature
                   site
                   (format "add-to-list-%d" index)))
                 (compare-var
                  (packlet--generated-symbol
                   "packlet--list-compare"
                   feature
                   site
                   (format "add-to-list-%d" index)))
                 (had-value-var
                  (packlet--generated-symbol
                   "packlet--list-bound"
                   feature
                   site
                   (format "add-to-list-%d" index)))
                 (added-var
                  (packlet--generated-symbol
                   "packlet--list-added"
                   feature
                   site
                   (format "add-to-list-%d" index))))
            `((defvar ,value-var nil)
              (defvar ,append-var nil)
              (defvar ,compare-var nil)
              (defvar ,had-value-var nil)
              (defvar ,added-var nil)
              (packlet--register-source-entry
               ',source-scope
               ',(list site kind index)
               (lambda ()
                 (setq ,value-var ,element
                       ,append-var ,append-form
                       ,compare-var ,compare-form
                       ,had-value-var (boundp ',variable))
                 (setq ,added-var
                       (not (and ,had-value-var
                                 (packlet--list-member-p
                                  ,value-var
                                  ,variable
                                  ,compare-var))))
                 (add-to-list ',variable ,value-var ,append-var ,compare-var))
               (lambda ()
                 (when (and ,added-var
                            (boundp ',variable))
                   (setq ,variable
                         (packlet--remove-list-entry
                          ,value-var
                          ,variable
                          ,append-var
                          ,compare-var))
                   (when (and (not ,had-value-var)
                              (boundp ',variable)
                              (null ,variable))
                     (makunbound ',variable)))
                 (packlet--unbind-variable ',value-var)
                 (packlet--unbind-variable ',append-var)
                 (packlet--unbind-variable ',compare-var)
                 (packlet--unbind-variable ',had-value-var)
                 (packlet--unbind-variable ',added-var))))))
       ,@(cl-loop
          for entry in enables
          for index from 0
          append
          (let* ((function (plist-get entry :function))
                 (arg-form (plist-get entry :arg))
                 (applied-var
                  (packlet--generated-symbol
                   "packlet--enable-applied"
                   feature
                   site
                   (format "enable-%d" index)))
                 (had-value-var
                  (packlet--generated-symbol
                   "packlet--enable-bound"
                   feature
                   site
                   (format "enable-%d" index)))
                 (previous-value-var
                  (packlet--generated-symbol
                   "packlet--enable-previous"
                   feature
                   site
                   (format "enable-%d" index)))
                 (enabled-value-var
                  (packlet--generated-symbol
                   "packlet--enable-current"
                   feature
                   site
                   (format "enable-%d" index)))
                 (enable-function
                  (packlet--generated-symbol
                   "packlet--enable"
                   feature
                   site
                   (format "enable-%d" index))))
            `((defvar ,applied-var nil)
              (defvar ,had-value-var nil)
              (defvar ,previous-value-var nil)
              (defvar ,enabled-value-var nil)
              (packlet--register-source-entry
               ',source-scope
               ',(list site :enable-state index)
               (lambda ()
                 (setq ,applied-var nil)
                 (packlet--unbind-variable ',had-value-var)
                 (packlet--unbind-variable ',previous-value-var)
                 (packlet--unbind-variable ',enabled-value-var)
                 (defalias ',enable-function
                   (lambda ()
                     (when (and (not ,applied-var)
                                (featurep ',feature)
                                (packlet--all-features-loaded-p ',afters))
                       (setq ,applied-var t
                             ,had-value-var (boundp ',function))
                       (if ,had-value-var
                           (setq ,previous-value-var ,function)
                         (packlet--unbind-variable ',previous-value-var))
                       (funcall ',function ,arg-form)
                       (unless (boundp ',function)
                         (error "packlet: %S did not leave a mode variable bound"
                                ',function))
                       (setq ,enabled-value-var ,function)))))
               (lambda ()
                 (when (and ,applied-var
                            (boundp ',function)
                            (equal ,function ,enabled-value-var))
                   (funcall ',function
                            (if (and ,had-value-var ,previous-value-var)
                                ,previous-value-var
                              0))
                   (when (and (not ,had-value-var)
                              (boundp ',function)
                              (null ,function))
                     (makunbound ',function)))
                 (packlet--unbind-variable ',applied-var)
                 (packlet--unbind-variable ',had-value-var)
                 (packlet--unbind-variable ',previous-value-var)
                 (packlet--unbind-variable ',enabled-value-var)
                 (packlet--unbind-function ',enable-function)))
              (packlet--register-after-load
               ',(list site :enable index)
               ',feature
               ',enable-function
               ',afters
               ,source-file))))
       ,@(cl-loop
          for prefix-map in prefix-maps
          for index from 0
          append
          (let ((created-var
                 (packlet--generated-symbol
                  "packlet--prefix-map-created"
                  feature
                  site
                  (format "prefix-map-%d" index)))
                (had-value-var
                 (packlet--generated-symbol
                  "packlet--prefix-map-bound"
                  feature
                  site
                  (format "prefix-map-%d" index)))
                (previous-value-var
                 (packlet--generated-symbol
                  "packlet--prefix-map-previous"
                  feature
                  site
                  (format "prefix-map-%d" index))))
            `((defvar ,created-var nil)
              (defvar ,had-value-var nil)
              (defvar ,previous-value-var nil)
              (packlet--register-source-entry
               ',source-scope
               ',(list site :prefix-map index)
               (lambda ()
                 (setq ,had-value-var (boundp ',prefix-map))
                 (if (and ,had-value-var
                          ,prefix-map)
                     (progn
                       (setq ,previous-value-var ,prefix-map
                             ,created-var nil)
                       (unless (keymapp ,prefix-map)
                         (error "packlet: %S does not name a keymap"
                                ',prefix-map)))
                   (setq ,created-var (make-sparse-keymap))
                   (set ',prefix-map ,created-var)
                   (if ,had-value-var
                       (setq ,previous-value-var nil)
                     (packlet--unbind-variable ',previous-value-var))))
               (lambda ()
                 (when (and ,created-var
                            (boundp ',prefix-map)
                            (eq ,prefix-map ,created-var))
                   (if ,had-value-var
                       (setq ,prefix-map ,previous-value-var)
                     (makunbound ',prefix-map)))
                 (packlet--unbind-variable ',created-var)
                 (packlet--unbind-variable ',had-value-var)
                 (packlet--unbind-variable ',previous-value-var))))))
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
       ,@(cl-loop
          for mode in modes
          for index from 0
          collect
          `(progn
             (packlet--maybe-autoload ',(cdr mode) ,file t)
             (packlet--register-source-entry
              ',source-scope
              ',(list site :mode index)
              (lambda ()
                (add-to-list 'auto-mode-alist ',mode))
              (lambda ()
                (setq auto-mode-alist
                      (delete ',mode auto-mode-alist))))))
       ,@(cl-loop
          for interpreter in interpreters
          for index from 0
          collect
          `(progn
             (packlet--maybe-autoload ',(cdr interpreter) ,file t)
             (packlet--register-source-entry
              ',source-scope
              ',(list site :interpreter index)
              (lambda ()
                (add-to-list 'interpreter-mode-alist ',interpreter))
              (lambda ()
                (setq interpreter-mode-alist
                      (delete ',interpreter interpreter-mode-alist))))))
       ,@(cl-loop
          for magic in magics
          for index from 0
          collect
          `(progn
             ,@(when (cdr magic)
                 `((packlet--maybe-autoload ',(cdr magic) ,file t)))
             (packlet--register-source-entry
              ',source-scope
              ',(list site :magic index)
              (lambda ()
                (add-to-list 'magic-mode-alist ',magic))
              (lambda ()
                (setq magic-mode-alist
                      (delete ',magic magic-mode-alist))))))
       ,@(cl-loop
          for magic in magic-fallbacks
          for index from 0
          collect
          `(progn
             ,@(when (cdr magic)
                 `((packlet--maybe-autoload ',(cdr magic) ,file t)))
             (packlet--register-source-entry
              ',source-scope
              ',(list site :magic-fallback index)
              (lambda ()
                (add-to-list 'magic-fallback-mode-alist ',magic))
              (lambda ()
                (setq magic-fallback-mode-alist
                      (delete ',magic magic-fallback-mode-alist))))))
       ,@(cl-loop
          for hook in hooks
          for index from 0
          append
          (let* ((hook-var (plist-get hook :hook))
                 (function (plist-get hook :function))
                 (delay (plist-get hook :delay))
                 (depth (plist-get hook :depth))
                 (local (plist-get hook :local))
                 (hook-function
                  (packlet--generated-symbol
                   "packlet--hook"
                   feature
                   site
                   (format "hook-%d" index)))
                 (hook-buffer-var
                  (and local
                       (packlet--generated-symbol
                        "packlet--hook-buffer"
                        feature
                        site
                        (format "hook-%d" index)))))
            (if delay
                (let ((timer-var
                       (packlet--generated-symbol
                        "packlet--delayed-hook-timer"
                        feature
                        site
                        (format "hook-%d" index))))
                  `((defvar ,timer-var nil)
                    ,@(when local
                        `((defvar ,hook-buffer-var nil)))
                    ,@(when (symbolp function)
                        `((packlet--maybe-autoload ',function ,file nil)))
                    (packlet--register-source-entry
                     ',source-scope
                     ',(list site :hook index)
                     (lambda ()
                       ,@(when local
                           `((setq ,hook-buffer-var (current-buffer))))
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
                       ,(if local
                            `(when (buffer-live-p ,hook-buffer-var)
                               (with-current-buffer ,hook-buffer-var
                                 (add-hook ',hook-var ',hook-function ,depth t)))
                          `(add-hook ',hook-var ',hook-function ,depth)))
                     (lambda ()
                       ,(if local
                            `(when (buffer-live-p ,hook-buffer-var)
                               (with-current-buffer ,hook-buffer-var
                                 (remove-hook ',hook-var ',hook-function t)))
                          `(remove-hook ',hook-var ',hook-function))
                       (when (boundp ',timer-var)
                         (when (timerp ,timer-var)
                           (cancel-timer ,timer-var))
                         (makunbound ',timer-var))
                       ,@(when local
                           `((packlet--unbind-variable ',hook-buffer-var)))
                       (packlet--unbind-function ',hook-function)))))
              `((progn
                  ,@(when local
                      `((defvar ,hook-buffer-var nil)))
                  ,@(when (symbolp function)
                      `((packlet--maybe-autoload ',function ,file nil)))
                  (packlet--register-source-entry
                   ',source-scope
                   ',(list site :hook index)
                   (lambda ()
                     ,@(when local
                         `((setq ,hook-buffer-var (current-buffer))))
                     (defalias ',hook-function
                       (lambda ()
                         (funcall ,(packlet--hook-function-form function))))
                     ,(if local
                          `(when (buffer-live-p ,hook-buffer-var)
                             (with-current-buffer ,hook-buffer-var
                               (add-hook ',hook-var ',hook-function ,depth t)))
                        `(add-hook ',hook-var ',hook-function ,depth)))
                   (lambda ()
                     ,(if local
                          `(when (buffer-live-p ,hook-buffer-var)
                             (with-current-buffer ,hook-buffer-var
                               (remove-hook ',hook-var ',hook-function t)))
                        `(remove-hook ',hook-var ',hook-function))
                     ,@(when local
                         `((packlet--unbind-variable ',hook-buffer-var)))
                     (packlet--unbind-function ',hook-function))))))))
       ,@(cl-loop
          for binding in bindings
          for index from 0
          collect
          (let ((binding-id (list site :bind index))
                (previous-binding-var
                 (packlet--generated-symbol
                  "packlet--previous-binding"
                  feature
                  site
                  (format "bind-%d" index))))
            (pcase binding
              (`(:global ,key ,command)
               `(progn
                  (defvar ,previous-binding-var nil)
                  (packlet--maybe-autoload ',command ,file t)
                  (packlet--register-source-entry
                   ',source-scope
                   ',binding-id
                   (lambda ()
                     (setq ,previous-binding-var
                           (lookup-key global-map ,(packlet--key-form key)))
                     (global-set-key
                      ,(packlet--key-form key)
                      ',command))
                   (lambda ()
                     (when (eq (lookup-key global-map ,(packlet--key-form key))
                               ',command)
                       (define-key global-map
                                   ,(packlet--key-form key)
                                   ,previous-binding-var))
                     (packlet--unbind-variable ',previous-binding-var)))))
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
          for binding in keymap-bindings
          for index from 0
          append
          (let* ((binding-id (list site :bind-keymap index))
                 (loader-command
                  (packlet--generated-symbol
                   "packlet--keymap-loader"
                   feature
                   site
                   (format "bind-keymap-%d" index)))
                 (command-entry-id (list site :bind-keymap-function index)))
            (pcase binding
              (`(:global ,key ,bound-keymap)
               (let ((target-keymap 'global-map))
                 `((packlet--register-source-entry
                    ',source-scope
                    ',command-entry-id
                    (lambda ()
                      (defalias ',loader-command
                        (lambda ()
                          (interactive)
                          (packlet--load-feature ',feature ,file)
                          (let ((target-map
                                 (packlet--resolve-keymap ',target-keymap))
                                (resolved-keymap
                                 (packlet--resolve-keymap ',bound-keymap)))
                            (unless target-map
                              (error "packlet: could not resolve keymap %S"
                                     ',target-keymap))
                            (unless resolved-keymap
                              (error "packlet: %S did not define keymap %S"
                                     ',feature ',bound-keymap))
                            (define-key target-map
                                        ,(packlet--key-form key)
                                        resolved-keymap)
                            (packlet--replay-command-keys)))))
                    (lambda ()
                      (packlet--unbind-function ',loader-command)))
                   (packlet--register-keymap-binding
                    ',binding-id
                    ',feature
                    ',target-keymap
                    ,(packlet--key-form key)
                    ',loader-command
                    ',afters
                    ,source-file
                    (lambda (current _map)
                      (eq current
                          (packlet--resolve-keymap ',bound-keymap)))))))
              (`(:map ,target-keymap ,key ,bound-keymap)
               `((packlet--register-source-entry
                  ',source-scope
                  ',command-entry-id
                  (lambda ()
                    (defalias ',loader-command
                      (lambda ()
                        (interactive)
                        (packlet--load-feature ',feature ,file)
                        (let ((target-map
                               (packlet--resolve-keymap ',target-keymap))
                              (resolved-keymap
                               (packlet--resolve-keymap ',bound-keymap)))
                          (unless target-map
                            (error "packlet: could not resolve keymap %S"
                                   ',target-keymap))
                          (unless resolved-keymap
                            (error "packlet: %S did not define keymap %S"
                                   ',feature ',bound-keymap))
                          (define-key target-map
                                      ,(packlet--key-form key)
                                      resolved-keymap)
                          (packlet--replay-command-keys)))))
                  (lambda ()
                    (packlet--unbind-function ',loader-command)))
                 (packlet--register-keymap-binding
                  ',binding-id
                  ',feature
                  ',target-keymap
                  ,(packlet--key-form key)
                  ',loader-command
                  ',afters
                  ,source-file
                  (lambda (current _map)
                    (eq current
                        (packlet--resolve-keymap ',bound-keymap)))))))))
       ,@(cl-loop
          for advice in advices
          for index from 0
          append
          (let* ((symbol (plist-get advice :symbol))
                 (how (plist-get advice :how))
                 (function (plist-get advice :function))
                 (name (plist-get advice :name))
                 (depth (plist-get advice :depth))
                 (advice-function
                  (packlet--generated-symbol
                   "packlet--advice"
                   feature
                   site
                   (format "advice-%d" index)))
                 (props (delq nil
                              (list (and name (cons 'name name))
                                    (and depth (cons 'depth depth))))))
            `((packlet--register-source-entry
               ',source-scope
               ',(list site :advice index)
               (lambda ()
                 ,@(when (symbolp function)
                     `((packlet--maybe-autoload ',function ,file nil)))
                 (defalias ',advice-function
                   ,(packlet--hook-function-form function))
                 (advice-add ',symbol ',how ',advice-function ',props))
               (lambda ()
                 (advice-remove ',symbol ',advice-function)
                 (packlet--unbind-function ',advice-function))))))
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
               (packlet--register-source-entry
                ',source-scope
                ',(list site :after-load-function index)
                (lambda ()
                  (defalias ',after-load-function
                    (lambda ()
                      ,@(cdr entry))))
                (lambda ()
                  (packlet--unbind-function ',after-load-function)))
               (packlet--register-after-load-handler
                ',(car entry)
                ',after-load-id
                ',after-load-function
                ,source-file))))
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
             (packlet--register-source-entry
              ',source-scope
              ',(list site :config-state)
              (lambda ()
                (setq ,configured-var nil)
                (defalias ',config-function
                  (lambda ()
                    (when (and (not ,configured-var)
                               (featurep ',feature)
                               (packlet--all-features-loaded-p ',afters))
                      (setq ,configured-var t)
                      ,@config-forms))))
              (lambda ()
                (packlet--unbind-variable ',configured-var)
                (packlet--unbind-function ',config-function)))
             (packlet--register-after-load
              ',(list site :config)
              ',feature
              ',config-function
              ',afters
              ,source-file)))
       ,@(when (packlet--has-section-p sections :demand)
           `((when ,demand-form
               (packlet--site-metadata-put ',site :demand-enabled t)
               (packlet--register-demand-load
                ',(list site :demand)
                ',feature
                ',afters
                ,file
                ,source-file)))))))

(provide 'packlet)

;;; packlet.el ends here
