;;; packlet-expand.el --- Macro expansion for packlet -*- lexical-binding: t; -*-

;; Copyright (C) 2026 SuzumiyaAoba
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Internal macro expansion helpers for `packlet'.

;;; Code:

(require 'packlet-parse)

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
         (derived-modes (packlet--normalize-derived-modes
                         (packlet--section sections :derived-mode)))
         (modes (packlet--normalize-pairs
                 (packlet--section sections :mode)
                 :mode
                 #'packlet--mode-entry-p))
         (remaps (packlet--normalize-pairs
                  (packlet--section sections :remap)
                  :remap
                  #'packlet--remap-entry-p))
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
         (faces (packlet--normalize-faces
                 (packlet--section sections :faces)))
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
          for entry in derived-modes
          for index from 0
          append
          (let* ((mode (plist-get entry :mode))
                 (parent (plist-get entry :parent))
                 (name (plist-get entry :name))
                 (docstring (plist-get entry :docstring))
                 (body (plist-get entry :body))
                 (variable-symbols (packlet--derived-mode-variable-symbols mode))
                 (previous-function-states-var
                  (packlet--generated-symbol
                   "packlet--derived-mode-previous-functions"
                   feature
                   site
                   (format "derived-mode-%d" index)))
                 (installed-function-states-var
                  (packlet--generated-symbol
                   "packlet--derived-mode-installed-functions"
                   feature
                   site
                   (format "derived-mode-%d" index)))
                 (previous-variable-states-var
                  (packlet--generated-symbol
                   "packlet--derived-mode-previous-variables"
                   feature
                   site
                   (format "derived-mode-%d" index)))
                 (installed-variable-states-var
                  (packlet--generated-symbol
                   "packlet--derived-mode-installed-variables"
                   feature
                   site
                   (format "derived-mode-%d" index))))
            `((defvar ,previous-function-states-var nil)
              (defvar ,installed-function-states-var nil)
              (defvar ,previous-variable-states-var nil)
              (defvar ,installed-variable-states-var nil)
              (packlet--register-source-entry
               ',source-scope
               ',(list site :derived-mode index)
               (lambda ()
                 (setq ,previous-function-states-var
                       (packlet--capture-function-states '(,mode))
                       ,previous-variable-states-var
                       (packlet--capture-variable-states ',variable-symbols))
                 (packlet--maybe-autoload ',parent ,file t)
                 (define-derived-mode ,mode ,parent ,name
                   ,@(when docstring (list docstring))
                   ,@body)
                 (setq ,installed-function-states-var
                       (packlet--capture-function-states '(,mode))
                       ,installed-variable-states-var
                       (packlet--capture-variable-states ',variable-symbols)))
               (lambda ()
                 (packlet--restore-variable-states
                  ,previous-variable-states-var
                  ,installed-variable-states-var)
                 (packlet--restore-function-states
                  ,previous-function-states-var
                  ,installed-function-states-var)
                 (packlet--unbind-variable ',previous-function-states-var)
                 (packlet--unbind-variable ',installed-function-states-var)
                 (packlet--unbind-variable ',previous-variable-states-var)
                 (packlet--unbind-variable ',installed-variable-states-var))))))
       ,@(cl-loop
          for remap in remaps
          for index from 0
          collect
          `(progn
             (packlet--maybe-autoload ',(cdr remap) ,file t)
             (packlet--register-source-entry
              ',source-scope
              ',(list site :remap index)
              (lambda ()
                (add-to-list 'major-mode-remap-alist ',remap))
              (lambda ()
                (setq major-mode-remap-alist
                      (delete ',remap major-mode-remap-alist))))))
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
          for entry in faces
          for index from 0
          append
          (let* ((face (plist-get entry :face))
                 (copy-source (plist-get entry :copy))
                 (attributes (plist-get entry :attributes))
                 (attributes-form
                  (cl-loop for (attribute value) on attributes by #'cddr
                           append (list attribute value)))
                 (previous-snapshot-var
                  (packlet--generated-symbol
                   "packlet--face-previous"
                   feature
                   site
                   (format "faces-%d" index)))
                 (applied-snapshot-var
                  (packlet--generated-symbol
                   "packlet--face-applied"
                   feature
                   site
                   (format "faces-%d" index)))
                 (attributes-var
                  (packlet--generated-symbol
                   "packlet--face-attributes"
                   feature
                   site
                   (format "faces-%d" index)))
                 (applied-var
                  (packlet--generated-symbol
                   "packlet--face-active"
                   feature
                   site
                   (format "faces-%d" index))))
            `((defvar ,previous-snapshot-var nil)
              (defvar ,applied-snapshot-var nil)
              (defvar ,attributes-var nil)
              (defvar ,applied-var nil)
              (packlet--register-source-entry
               ',source-scope
               ',(list site :faces-state index)
               (lambda ()
                 (setq ,applied-var nil)
                 (packlet--unbind-variable ',previous-snapshot-var)
                 (packlet--unbind-variable ',applied-snapshot-var)
                 (packlet--unbind-variable ',attributes-var))
               (lambda ()
                 (when (and ,applied-var
                            (facep ',face)
                            ,previous-snapshot-var
                            ,applied-snapshot-var)
                   (packlet--restore-face-snapshot
                    ',face
                    ,previous-snapshot-var
                    ,applied-snapshot-var))
                 (packlet--unbind-variable ',previous-snapshot-var)
                 (packlet--unbind-variable ',applied-snapshot-var)
                 (packlet--unbind-variable ',attributes-var)
                 (packlet--unbind-variable ',applied-var)))
              (packlet--register-after-load
               ',(list site :faces index)
               ',feature
               (lambda ()
                 (when (and (not ,applied-var)
                            (featurep ',feature)
                            (packlet--all-features-loaded-p ',afters))
                   (unless (facep ',face)
                     (make-empty-face ',face))
                   (setq ,applied-var t
                         ,previous-snapshot-var
                         (packlet--face-snapshot ',face)
                         ,attributes-var
                         (list ,@attributes-form))
                   ,@(when copy-source
                       `((unless (facep ',copy-source)
                           (error "packlet: %S is not a face" ',copy-source))
                         (copy-face ',copy-source ',face)))
                   (when ,attributes-var
                     (apply #'set-face-attribute ',face nil ,attributes-var))
                   (setq ,applied-snapshot-var
                         (packlet--face-snapshot ',face))))
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

(provide 'packlet-expand)

;;; packlet-expand.el ends here
