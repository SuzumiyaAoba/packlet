;;; packlet-check.el --- Non-evaluating diagnostics for packlet -*- lexical-binding: t; -*-

;; Copyright (C) 2026 SuzumiyaAoba
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Check literal declarations without expanding macros or evaluating user code.

;;; Code:

(require 'packlet-parse)
(require 'lisp-mode)

(defconst packlet--check-normalizers
  '((:setq packlet--normalize-customs)
    (:custom packlet--normalize-customs)
    (:load packlet--normalize-loads)
    (:add-to-list packlet--normalize-add-to-lists)
    (:list packlet--normalize-lists-alias)
    (:alist packlet--normalize-alists)
    (:commands packlet--normalize-symbols :commands)
    (:autoload packlet--normalize-autoloads)
    (:mode packlet--normalize-pairs :mode packlet--mode-entry-p)
    (:remap packlet--normalize-pairs :remap packlet--remap-entry-p)
    (:derived-mode packlet--normalize-derived-modes)
    (:hook packlet--normalize-hooks)
    (:hook-setq packlet--normalize-hook-setqs)
    (:hook-call packlet--normalize-hook-calls)
    (:hook-add packlet--normalize-hook-adds)
    (:hook-enable packlet--normalize-hook-enables)
    (:hook-disable packlet--normalize-hook-disables)
    (:hook-when packlet--normalize-hook-whens)
    (:hook-if-feature packlet--normalize-hook-if-features)
    (:startup packlet--normalize-startups)
    (:startup-enable packlet--normalize-startup-enables)
    (:bind packlet--normalize-bindings)
    (:bind-keymap packlet--normalize-bindings)
    (:bind-after-load packlet--normalize-bind-after-loads)
    (:prefix-map packlet--normalize-symbols :prefix-map)
    (:enable packlet--normalize-enables)
    (:faces packlet--normalize-faces)
    (:advice packlet--normalize-advices)
    (:interpreter packlet--normalize-pairs :interpreter packlet--mode-entry-p)
    (:magic packlet--normalize-pairs :magic packlet--magic-entry-p)
    (:magic-fallback packlet--normalize-pairs :magic-fallback packlet--magic-entry-p)
    (:after packlet--normalize-afters)
    (:after-load packlet--normalize-after-loads)
    (:functions packlet--normalize-symbols :functions)
    (:defines packlet--normalize-symbols :defines))
  "Built-in normalizers safe to run without evaluating configuration forms.")

(defun packlet--check-validate (form)
  "Validate literal declaration FORM without invoking user keyword callbacks."
  (unless (and (packlet--proper-list-p form)
               (symbolp (cadr form)) (cadr form)
               (not (keywordp (cadr form))))
    (error "packlet: FEATURE must be a non-nil symbol"))
  (let* ((feature (cadr form))
         (sections (packlet--parse-body (cddr form)))
         (id (packlet--id-form sections))
         (file (packlet--file-form sections feature))
         (idle (packlet--idle-form sections))
         (demand (packlet--demand-form sections)))
    (packlet--guard-form sections)
    (when (or (numberp idle) (memq idle '(nil t)))
      (packlet--idle-delay idle))
    (when (and (assq :cleanup sections)
               (not (packlet--section sections :config)))
      (error "packlet: :cleanup requires a non-empty :config"))
    (dolist (section sections)
      (when-let* ((normalizer (assq (car section) packlet--check-normalizers)))
        (apply (cadr normalizer) (cdr section) (cddr normalizer))))
    (list :feature feature :id id :file file :sections sections
          :afters (packlet--normalize-afters (packlet--section sections :after))
          :demand demand)))

(defun packlet--check-keyword-suggestion (keyword)
  "Return a spelling suggestion for unknown KEYWORD, or nil."
  (let ((distance 4) candidate)
    (dolist (known (append packlet--keywords (mapcar #'car packlet--user-keywords)))
      (let ((current (string-distance (symbol-name keyword) (symbol-name known))))
        (when (< current distance)
          (setq distance current candidate known))))
    candidate))

(defun packlet--check-bindings (sections)
  "Return literal (MAP KEY) binding identities in SECTIONS."
  (let (result)
    (dolist (keyword '(:bind :bind-keymap))
      (dolist (binding (packlet--normalize-bindings
                       (packlet--section sections keyword)))
        (pcase binding
          (`(:global ,key ,_) (push (list 'global-map key) result))
          (`(:map ,map ,key ,_) (push (list map key) result)))))
    (dolist (binding (packlet--normalize-bind-after-loads
                     (packlet--section sections :bind-after-load)))
      (push (list (plist-get binding :keymap) (plist-get binding :key)) result))
    (mapcar (lambda (binding)
              (list (car binding)
                    (key-description (if (stringp (cadr binding))
                                         (kbd (cadr binding))
                                       (cadr binding)))))
            (nreverse result))))

(defun packlet--check-blocked-demands (declarations)
  "Return DECLARATIONS potentially blocked by demand dependency cycles.
Treat dependencies outside the demand graph as independently satisfiable.
Multiple declarations of a feature are alternative ways to load it."
  (let* ((demands (cl-remove-if-not (lambda (entry) (plist-get entry :demand))
                                   declarations))
         (features (mapcar (lambda (entry) (plist-get entry :feature)) demands))
         (ready nil)
         changed)
    (setq changed t)
    (while changed
      (setq changed nil)
      (dolist (entry demands)
        (let ((feature (plist-get entry :feature)))
          (when (and (not (memq feature ready))
                     (or (featurep feature)
                         (cl-every
                          (lambda (expression)
                            (packlet--dependency-satisfied-p
                             expression
                             (lambda (dependency)
                               (or (featurep dependency)
                                   (not (memq dependency features))
                                   (memq dependency ready)))))
                          (plist-get entry :afters))))
            (push feature ready)
            (setq changed t)))))
    (cl-remove-if (lambda (entry) (memq (plist-get entry :feature) ready)) demands)))

(defun packlet--check-buffer ()
  "Return diagnostics for the current temporary input buffer."
  (let ((read-circle nil)
        (ids (make-hash-table :test #'equal))
        (bindings (make-hash-table :test #'equal))
        declarations diagnostics)
    (cl-labels
        ((report (severity code message line &optional declaration)
           (push (list :severity severity :code code :message message :line line
                       :feature (plist-get declaration :feature)
                       :id (plist-get declaration :id))
                 diagnostics))
         (inspect (form line)
           (condition-case err
               (let ((unknown (cl-find-if
                               (lambda (item)
                                 (and (keywordp item) (not (packlet--keyword-p item))))
                               (cddr form))))
                 (if unknown
                     (report :error :unknown-keyword
                             (format "Unknown keyword %S%s" unknown
                                     (if-let* ((hint (packlet--check-keyword-suggestion unknown)))
                                         (format "; did you mean %S?" hint) ""))
                             line)
                   (let* ((entry (packlet--check-validate form))
                          (id (plist-get entry :id))
                          (feature (plist-get entry :feature))
                          (file (plist-get entry :file)))
                     (setq entry (plist-put entry :line line))
                     (push entry declarations)
                     (when id
                       (when-let* ((previous (gethash id ids)))
                         (report :error :duplicate-id
                                 (format "Duplicate :id %S (first declared on line %d)" id previous)
                                 line entry))
                       (puthash id line ids))
                     (dolist (binding (packlet--check-bindings (plist-get entry :sections)))
                       (when-let* ((previous (gethash binding bindings)))
                         (report :warning :duplicate-binding
                                 (format "Binding %S is also declared on line %d" binding previous)
                                 line entry))
                       (puthash binding line bindings))
                     (unless (or (featurep feature) (locate-library file))
                       (report :warning :missing-library
                               (format "Library %S for %S is not on load-path; check :file or package activation"
                                       file feature)
                               line entry))
                     (dolist (dependency (packlet--after-features (plist-get entry :afters)))
                       (unless (or (featurep dependency)
                                   (locate-library (symbol-name dependency)))
                         (report :warning :missing-dependency-library
                                 (format "No library named %S; the feature may be provided by another library"
                                         dependency)
                                 line entry))))))
             (error (report :error :invalid-declaration (error-message-string err) line))))
         (walk (form line)
           (when (consp form)
             (cond
              ((eq (car form) 'packlet) (inspect form line))
              ((memq (car form) '(quote function)) nil)
              ((eq (car form) (intern "`")) nil)
              (t (let ((rest form))
                   (while (consp rest)
                     (walk (pop rest) line))))))))
      (goto-char (point-min))
      (condition-case err
          (while (progn (forward-comment (point-max)) (not (eobp)))
            (let ((line (line-number-at-pos)))
              (condition-case read-error
                  (walk (read (current-buffer)) line)
                (error
                 (report :error :read-error (error-message-string read-error) line)
                 (goto-char (point-max))))))
        (error (report :error :read-error (error-message-string err) (line-number-at-pos))))
      (dolist (entry (packlet--check-blocked-demands declarations))
        (report :warning :demand-cycle
                (format "Demand for %S may be blocked by a dependency cycle among checked declarations"
                        (plist-get entry :feature))
                (plist-get entry :line) entry)))
    (sort (nreverse diagnostics)
          (lambda (left right) (< (plist-get left :line) (plist-get right :line))))))

;;;###autoload
(defun packlet-check (&optional source)
  "Check literal declarations in SOURCE without evaluating configuration code.
SOURCE is a buffer, a file name, or nil for the current buffer.  Return a list
of diagnostic plists with :severity, :code, :message, :line, :feature, and :id.
Nested declarations use their enclosing top-level form's line number.
No user macros, keyword callbacks, guards, or configuration forms are run.
Quoted data is skipped.  Library and dependency warnings are advisory."
  (interactive)
  (let* ((source (or source (current-buffer)))
         (diagnostics
          (with-temp-buffer
            (set-syntax-table emacs-lisp-mode-syntax-table)
            (cond
             ((bufferp source)
              (setq default-directory (buffer-local-value 'default-directory source))
              (insert-buffer-substring source))
             ((stringp source)
              (setq default-directory (file-name-directory (expand-file-name source)))
              (insert-file-contents source))
             (t (error "packlet: SOURCE must be a buffer or file name")))
            (packlet--check-buffer))))
    (when (called-interactively-p 'interactive)
      (with-help-window "*Packlet Check*"
        (princ (if diagnostics
                   (mapconcat
                    (lambda (entry)
                      (format "%d: %s: %s" (plist-get entry :line)
                              (plist-get entry :severity) (plist-get entry :message)))
                    diagnostics "\n")
                 "No packlet diagnostics."))))
    diagnostics))

(provide 'packlet-check)

;;; packlet-check.el ends here
