;;; packlet-source.el --- Source tracking for packlet -*- lexical-binding: t; -*-

;; Copyright (C) 2026 SuzumiyaAoba
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Internal source tracking, reevaluation, and inspection helpers for `packlet'.

;;; Code:

(require 'packlet-runtime)
(require 'packlet-parse)

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
    (`(named ,_ ,_) t)
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
  (packlet--missing-afters (plist-get metadata :afters)))

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
                  (when (plist-get metadata :id)
                    (format "Declaration: %S" (plist-get metadata :id)))
                  (format "Entries: %d" (length entries))
                  (format "Afters: %s"
                          (if-let ((afters (plist-get metadata :afters)))
                              (mapconcat (lambda (expression)
                                           (if (symbolp expression)
                                               (symbol-name expression)
                                             (prin1-to-string expression)))
                                         afters ", ")
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

(defun packlet--replace-site-entries (entries site replacements)
  "Replace SITE's entries in ENTRIES with REPLACEMENTS, preserving other sites."
  (let (result inserted)
    (dolist (entry entries)
      (if (equal (packlet--find-entry-site (packlet--source-entry-id entry)) site)
          (unless inserted
            (setq result (append result replacements)
                  inserted t))
        (setq result (append result (list entry)))))
    (if inserted result (append result replacements))))

(defun packlet--read-declaration-id ()
  "Read a registered declaration ID from the current source."
  (let (choices)
    (dolist (entry (packlet--source-entries (packlet--buffer-source-scope)))
      (let ((site (packlet--find-entry-site (packlet--source-entry-id entry))))
        (when (eq (car-safe site) 'named)
          (cl-pushnew (cons (prin1-to-string (nth 2 site)) (nth 2 site))
                      choices :test #'equal))))
    (unless choices
      (user-error "No named packlet declarations in this source"))
    (cdr (assoc (completing-read "Declaration: " choices nil t) choices))))

;;;###autoload
(defun packlet-cleanup-declaration (id &optional source)
  "Clean up declaration ID in SOURCE, leaving other declarations untouched.
SOURCE has the same meaning as in `packlet-cleanup-source'.
Return entries whose cleanup failed, retaining them for a later retry."
  (interactive (list (packlet--read-declaration-id)))
  (let* ((scope (packlet--resolve-source-scope source))
         (site (packlet--named-site scope id))
         (entries (packlet--source-entries scope))
         (selected (packlet--site-entries site entries))
         (failed (packlet--run-source-entry-cleanups selected scope)))
    (packlet--set-source-entries
     scope (packlet--replace-site-entries entries site failed))
    failed))

;;;###autoload
(defun packlet-eval-declaration (&optional form lexical)
  "Evaluate named packlet FORM transactionally without replacing other sites.
Interactively, or when FORM is nil, read the top-level declaration at point.
FORM must have an explicit :id.  LEXICAL is passed to `eval'; interactive
calls use the current buffer's `lexical-binding'."
  (interactive (list nil lexical-binding))
  (unless form
    (setq form (save-excursion
                 (unless (looking-at-p "(packlet\\_>")
                   (beginning-of-defun))
                 (read (current-buffer)))))
  (unless (and (packlet--proper-list-p form) (eq (car-safe form) 'packlet))
    (user-error "Expected a top-level packlet declaration"))
  (let* ((id (packlet--id-form (packlet--parse-body (cddr form))))
         (scope (packlet--buffer-source-scope)))
    (unless id
      (user-error "Declaration-level evaluation requires :id"))
    (packlet--with-source-session
     scope nil (lambda () (eval form lexical))
     (packlet--named-site scope id))))

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

(defun packlet--with-source-session (scope expansion-file thunk &optional site)
  "Evaluate THUNK transactionally for SOURCE SCOPE.
When EXPANSION-FILE is non-nil, reset its expansion counter for the session.
When SITE is non-nil, replace only that declaration's entries."
  (let ((scope (packlet--normalize-source-scope scope)))
    (if (or (null scope) packlet--active-source-scope)
        (funcall thunk)
      (let* ((all-entries (packlet--source-entries scope))
             (old-entries (if site (packlet--site-entries site all-entries)
                            all-entries))
             (old-expansion-state
              (and expansion-file
                   (gethash expansion-file packlet--expansion-load-states)))
             (old-cleanup-failures
              (packlet--run-source-entry-cleanups old-entries scope))
             (store (lambda (entries)
                      (packlet--set-source-entries
                       scope (if site
                                 (packlet--replace-site-entries all-entries site entries)
                               entries)))))
        (funcall store old-cleanup-failures)
        (when expansion-file
          (remhash expansion-file packlet--expansion-load-states))
        (let ((packlet--active-source-scope scope)
              (packlet--pending-source-entries nil))
          (condition-case err
              (let ((result (funcall thunk)))
                (funcall store
                         (packlet--merge-source-entry-lists
                          old-cleanup-failures packlet--pending-source-entries))
                result)
            (error
             (let ((failed (packlet--run-source-entry-cleanups
                            packlet--pending-source-entries scope)))
               (setq packlet--pending-source-entries nil)
               (packlet--run-source-entry-installs old-entries scope)
               (funcall store (packlet--merge-source-entry-lists old-entries failed)))
             (when expansion-file
               (if old-expansion-state
                   (puthash expansion-file old-expansion-state
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
      (let* ((id (and (eq (car-safe form) 'packlet)
                      (packlet--id-form (packlet--parse-body (cddr form)))))
             (scope (if id (packlet--buffer-source-scope)
                      (packlet--current-eval-scope))))
        (packlet--with-source-session
         scope nil (lambda () (funcall orig form lexical))
         (and id (packlet--named-site scope id)))))))

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

(provide 'packlet-source)

;;; packlet-source.el ends here
