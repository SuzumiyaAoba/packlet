# packlet

`packlet` is a small DSL for lazy-first Emacs package setup.

It focuses on the common parts of package configuration:

- autoloading interactive commands
- autoloading helper functions
- registering one-off list and alist entries
- registering `auto-mode-alist` entries
- registering `major-mode-remap-alist` entries
- registering `interpreter-mode-alist` entries
- registering `magic-mode-alist` entries
- registering `magic-fallback-mode-alist` entries
- defining derived major modes
- wiring hooks, common hook-driven setup, global key bindings, and package keymaps
- gating setup behind runtime conditions
- running one-off startup callbacks and startup-time mode enables
- defining prefix keymaps for package-local bindings
- wiring lazy prefix-key keymaps
- enabling mode-like functions after their feature loads
- configuring faces after their feature loads
- registering function advice
- deferring configuration until a feature is actually loaded
- warming up a feature after startup during idle time
- optionally demanding a feature once its dependencies are ready
- re-evaluating `packlet` forms transactionally without stacking stale handlers
- declaring external functions and variables for byte compilation

It does not install packages by itself. `packlet` only describes when and how
configuration should become active.

When a package ships `foo-autoloads.el`, `packlet` loads that first and only
falls back to `foo.el` when a symbol is still undefined. This lets commands in
sub-libraries such as `magit-status` resolve to the right file without extra
wrapper code.

## Requirements

- Emacs 29.1 or newer

## Installation

### package-vc

```elisp
(package-vc-install "https://github.com/SuzumiyaAoba/packlet")
```

### Manual

Clone this repository and add it to `load-path`:

```elisp
(add-to-list 'load-path "/path/to/packlet")
(require 'packlet)
```

## Example

```elisp
(require 'packlet)

(packlet magit
  :commands (magit-status magit-blame-addition)
  :functions magit-display-buffer-same-window-except-diff-v1
  :defines magit-display-buffer-function
  :bind ("C-x g" . magit-status)
  :after project
  :idle 2.0
  :custom
  (magit-display-buffer-function
   #'magit-display-buffer-same-window-except-diff-v1))
```

## Reevaluation

`packlet` treats reevaluation as a first-class workflow.

- Re-evaluating a file with `eval-buffer` or `load-file` replaces old
  `:setq`, `:custom`, `:add-to-list`, `:list`, `:alist`, `:config`,
  `:hook`, `:hook-setq`, `:hook-call`, `:hook-add`, `:hook-enable`,
  `:hook-disable`,
  `:hook-when`, `:hook-if-feature`,
  `:startup`, `:startup-enable`, `:bind`, `:bind-keymap`, `:bind-after-load`, `:prefix-map`,
  `:enable`, `:faces`, `:advice`,
  `:mode`, `:remap`, `:derived-mode`, `:interpreter`, `:magic`,
  `:magic-fallback`, `:after-load`, `:idle`, and `:demand` registrations
  from that source instead of stacking duplicates.
- File-backed reevaluation is transactional. If the new evaluation fails part
  way through, `packlet` cleans up partially installed registrations and restores
  the previously working registrations.
- Mode alist cleanup removes only entries added by the declaration, preserving
  matching entries that were already present.
- Direct `eval` is also tracked. In Lisp buffers, nested forms containing
  `packlet` are detected. In non-Lisp buffers, top-level `packlet` forms are
  tracked.
- Non-file `eval` registrations are scoped to the current buffer and are
  cleaned up automatically when that buffer is killed.
- `packlet-describe-source` shows the registrations currently owned by a file
  or buffer scope.
- `packlet-describe-feature` shows the registrations currently associated with
  a feature across source scopes.
- `packlet-explain-feature` shows the current runtime state for a feature,
  including pending `:after`, `:config`, `:demand`, and `:idle` status.
- `packlet-list-features` lists the features currently registered through
  `packlet`.
- `packlet-cleanup-source` removes the registrations currently owned by a file
  or buffer scope.

### Named declarations

Use a literal `:id` (a non-nil symbol other than `t`, or a non-empty string) to
identify a declaration independently of its position or body. IDs must be unique
within their file or non-file buffer; different sources can use the same ID.

```elisp
(packlet org
  :id org-editing
  :hook-enable (org-mode-hook visual-line-mode))
```

- `M-x packlet-eval-declaration` evaluates the top-level named declaration at
  point, replacing only that declaration's registrations. On failure, its old
  registrations are restored; other declarations are left in place.
- Direct `eval` of a top-level `packlet` form with `:id` also replaces just that
  declaration. Anonymous or enclosing forms retain the existing source-wide
  evaluation behavior. Use the explicit command when editing one declaration.
- `M-x packlet-cleanup-declaration` removes a named declaration from the current
  source. Lisp callers can use `(packlet-cleanup-declaration 'org-editing source)`;
  `source` accepts the same arguments as `packlet-cleanup-source`.
- Whole-file `eval-buffer` and `load-file` still replace the entire source,
  including declarations that were removed from the file.
- Renaming an ID creates a different declaration. Clean up the old ID explicitly
  or reevaluate the whole source. IDs appear in the source inspection commands
  and in `packlet-explain-feature`.

### Explicit configuration cleanup

Use `:cleanup` to undo effects of custom `:config` code that packlet cannot infer:

```elisp
(packlet foo
  :id foo-session
  :config
  (my-foo-start)
  :cleanup
  (my-foo-stop))
```

`:cleanup` requires a non-empty `:config`. It becomes active when `:config`
**starts**, so it also handles partially failed configuration. It does not run
for a false guard or a configuration still waiting for its dependencies. Cleanup
runs on removal, replacement, buffer cleanup, or rollback, before earlier
registrations in that declaration are undone. Successful cleanup is not repeated;
failed cleanup is reported and retained for retry by the cleanup commands.

Cleanup code should tolerate partially initialized state and repeated attempts.
Rollback may run the old `:config` again to restore its registrations. Arbitrary
`:init`/`:load` side effects and external resources are not automatically
transactional; `:cleanup` is an explicit aid, not a guarantee of complete undo.

## Diagnostics

`M-x packlet-check` checks the current buffer without evaluating its code or
loading the packages being checked. Lisp callers can pass a buffer or file name:

```elisp
(packlet-check "~/.emacs.d/init.el")
```

It reports:

- unknown keywords, with spelling suggestions, and invalid built-in arguments
- duplicate IDs and potential duplicate bindings (including delayed bindings)
- libraries not found on the current `load-path`, with `:file`/activation hints
- potentially blocked `:demand` dependency cycles, accounting for OR alternatives

The return value is a list of diagnostic plists with `:severity`, `:code`,
`:message`, `:line`, `:feature`, and `:id`. Literal declarations nested in forms
such as `progn` are checked using the enclosing top-level form's line number.
Quoted data and function-quoted templates are skipped. The checker does not
expand macros, invoke user keyword callbacks, or evaluate conditions; custom
keyword payloads and generated declarations cannot be fully validated. Library,
binding, and dependency warnings are advisory: guards may be mutually exclusive,
features may live in differently named libraries, and external code may load them.

Unknown bare keywords are also rejected during normal macro expansion. Quote a
literal keyword value used as a top-level section form, for example
`:init ':some-value`, so it is not mistaken for a section name.

## Compiled configurations

For immutable, generated configurations, bind `packlet-expand-source-tracking`
to `nil` while byte-compiling. This emits direct settings and omits source
metadata, cleanup closures, and registration calls. The default remains `t` for
interactive editing; code compiled with tracking disabled cannot be rolled back
or removed with the source cleanup commands. This also disables `:cleanup` and
named-declaration inspection/cleanup for the compiled configuration.

```elisp
(require 'packlet)
(let ((packlet-expand-source-tracking nil))
  (byte-compile-file "init.el"))
```

The compiled init file can use `(eval-when-compile (require 'packlet))` and
`(require 'packlet-runtime)` to leave parsing and source tracking out of startup.
The runtime includes the helpers needed by both `:idle` and `:demand`.
Generated helper names use a stable digest so the same expansion site has the
same names across fresh Emacs processes.

When a package manager already supplies autoloads, set
`packlet-load-package-autoloads` to `nil` to avoid repeated autoload-file lookups.
Fallback autoloads still work; commands in sub-libraries must already be
autoloaded or declare their file explicitly with `:autoload`.

## Keywords

- `:id`
  A literal name unique within the source, used for declaration-level
  reevaluation and cleanup. See [Named declarations](#named-declarations).
- `:file`
  Override the library name used for autoloads, declarations, and demand
  loading. By default this is the same as the feature symbol.
- `:when`
  Evaluate the entire `packlet` form only when the condition is non-nil.
- `:unless`
  Evaluate the entire `packlet` form only when the condition is nil.
- `:init`
  Forms evaluated immediately.
- `:setq`
  `(variable value)` forms applied immediately with `setq`.
- `:custom`
  `(variable value)` forms applied immediately with `setopt`.
- `:load`
  Load helper libraries immediately. If a library cannot be found, `packlet`
  emits a warning.
- `:add-to-list`
  Lower-level alias for `:list`.
- `:list`
  `(variable element)` forms applied immediately with `add-to-list`.
  `:list` and `:add-to-list` also accept multiple elements per entry, for
  example `(exec-path-from-shell-variables "A" "B" "C")`.
  List-style entries additionally accept `:append` and `:compare`, for
  example `(completion-at-point-functions #'cape-file :append t)`.
- `:alist`
  `(variable element)` forms applied immediately with `add-to-list`, using
  the entry key (`car`) as the default equality check.  This is useful for
  alists such as `display-buffer-alist`.
- `:config`
  Forms evaluated once after the feature is loaded and the `:after` expressions
  are satisfied.
- `:cleanup`
  Forms that undo custom `:config` effects after that configuration has started.
  See [Explicit configuration cleanup](#explicit-configuration-cleanup).
- `:commands`
  Symbols to autoload. `packlet` prefers package autoload definitions when
  available and otherwise falls back to the package file.
- `:autoload`
  Function symbols to autoload with the same resolution rules as `:commands`.
  A bare symbol is registered as non-interactive from the package file.
  A tuple `(function "file")` autoloads from a specific file as non-interactive.
  A tuple `(function "file" t)` autoloads from a specific file as interactive.
- `:mode`
  `("\\\\.ext\\\\'" . some-mode)` pairs added to `auto-mode-alist`.
- `:remap`
  `(old-mode . new-mode)` pairs added to `major-mode-remap-alist`.
- `:derived-mode`
  `(child-mode parent-mode "Name" body...)` entries expanded with
  `define-derived-mode`.
- `:interpreter`
  `("python3" . python-mode)` pairs added to `interpreter-mode-alist`.
- `:magic`
  `(regexp . mode)` or `(match-function . mode)` pairs added to
  `magic-mode-alist`.
- `:magic-fallback`
  `(regexp . mode)` or `(match-function . mode)` pairs added to
  `magic-fallback-mode-alist`.
- `:hook`
  `(some-hook . some-function)` pairs added with `add-hook`.
  Hook arguments are forwarded to the function, and immediate calls preserve
  its return value. You can also use `(some-hook some-function delay)` or
  `(some-hook some-function :delay delay)` to run the function from an idle timer
  after the hook fires, retaining the hook arguments. Specify only one delay
  form. List-style entries additionally accept
  `:append t`, `:depth N`, and `:local t`, for example
  `(some-hook some-function :append t)` or
  `(some-hook some-function delay :depth -10 :local t)`.
- `:hook-setq`
  `(some-hook (variable value) ...)` entries that add a hook function calling
  `setq-local` for each variable. This is useful for mode hooks such as
  `(python-mode-hook (python-indent-offset 4) (fill-column 88))`.
  Trailing `:delay`, `:append`, `:depth`, and `:local` options follow the
  same meaning as `:hook`, for example
  `(org-mode-hook (truncate-lines nil) :delay 0.5)`.
- `:hook-call`
  `(some-hook function arg...)` entries that call `function` with `arg...`
  when the hook runs. This is useful when a hook only needs a direct function
  call, for example
  `(window-setup-hook set-frame-parameter nil 'fullscreen 'fullboth)`.
  Trailing `:delay`, `:append`, `:depth`, and `:local` options are also
  supported. This and the other hook setup helpers use their declared settings
  or arguments rather than forwarding hook arguments.
- `:hook-add`
  `(some-hook target-hook function)` entries that add `function` to
  `target-hook` when `some-hook` runs. `function` must be a symbol.
  List-style entries additionally accept `:append t` and `:local t`, for
  example `(go-mode-hook before-save-hook gofmt :local t)`.
- `:hook-enable`
  `(some-hook function)` or `(some-hook function arg)` entries that call a
  mode-like function from a hook. This is useful for patterns such as
  `(prog-mode-hook display-line-numbers-mode)`.
- `:hook-disable`
  `(some-hook function)` or `(some-hook function arg)` entries that call a
  mode-like function with a disabling argument from a hook. The default
  argument is `-1`, so this is useful for patterns such as
  `(org-mode-hook display-line-numbers-mode)`.
- `:hook-when`
  `(some-hook condition function)` entries that call `function` from the hook
  only when `condition` is non-nil at hook run time (or timer run time when
  delayed). Hook arguments are forwarded. The same positional delay and trailing
  `:delay`, `:append`, `:depth`, and `:local` options as `:hook` are supported.
- `:hook-if-feature`
  `(some-hook feature function)` entries that call `function` from the hook
  only when `feature` is currently loaded. This is a shorthand for
  `:hook-when` with `(featurep 'feature)`.
- `:startup`
  `function` or `(function arg...)` entries that run once from
  `after-init-hook`. If startup already finished when the form is evaluated,
  `packlet` runs the callback immediately.
- `:startup-enable`
  `function` or `(function arg)` entries that enable a mode-like function
  once at startup. If startup already finished when the form is evaluated,
  `packlet` applies the mode immediately and restores the previous state on
  cleanup.
- `:bind`
  Global key bindings such as `("C-c p" . some-command)` or keymap groups such
  as `(:map some-mode-map ("C-c p" . some-command))`.
- `:bind-keymap`
  Lazy prefix-key bindings such as `("C-c p" . projectile-command-map)` or
  keymap groups such as `(:map some-mode-map ("C-c p" . some-prefix-map))`.
  The first key press loads the feature, swaps in the real keymap, and replays
  the original key sequence.
- `:bind-after-load`
  `(feature binding...)` entries that install ordinary `:bind` bindings only
  after `feature` loads. This is useful for patching package-owned keymaps such
  as `projectile-command-map` without writing `with-eval-after-load` manually.
- `:prefix-map`
  Symbols naming sparse keymaps that should be created when still unbound.
  This is useful together with `:bind-keymap` and `:bind (:map ...)` when a
  package uses a config-owned prefix map.
- `:enable`
  Symbols naming mode-like functions to call after the feature is loaded and
  the `:after` expressions are satisfied.  A bare symbol calls the function with `1`.
  A tuple `(function arg)` calls it with `arg`, for example
  `(treemacs-git-mode 'deferred)`.
- `:faces`
  `(face :attribute value ...)` entries applied after the feature is loaded and
  the `:after` expressions are satisfied.  `:copy` copies another face before applying
  explicit attributes, for example
  `(anzu-mode-line :copy mode-line)` or
  `(git-gutter:modified :background "purple")`.
- `:advice`
  `(target how function)` entries added with `advice-add`.
  List-style entries additionally accept `:name` and `:depth`, for example
  `(projectile-find-file :override consult-projectile-find-file :depth -10)`.
- `:after`
  Dependency expressions that must be satisfied before `:config`, `:demand`,
  `:idle`, `:enable`, or `:faces` becomes active. Bare symbols and legacy lists
  remain AND conditions: `:after (a b)` requires both. `:after (:or a b)` accepts
  either feature, and `:after (:and a (:or b c))` requires `a` and either `b` or
  `c`. Groups must be non-empty. Multiple top-level expressions are combined
  with AND. These expressions test loaded features; they do not load dependencies.
  Keymap bindings watch all mentioned features for map availability, as before.
- `:after-load`
  `(feature body...)` forms evaluated after an arbitrary feature loads, even if
  it is not the package feature being configured. If the feature is already
  loaded, the body runs once immediately on registration.
- `:idle`
  Require the feature after startup on the next idle period. With no value,
  this defaults to `1.0`. A numeric value changes the idle delay in seconds.
  If the minibuffer is active or input is pending, `packlet` retries on a
  later idle period instead of loading immediately. Missing libraries emit a
  warning when the idle load runs.
- `:demand`
  Require the feature once its `:after` dependencies are satisfied. With no
  value, this defaults to `t`. You can also pass a condition form. Missing
  libraries emit a warning.
- `:functions`
  Function symbols declared with `declare-function` for byte compilation.
- `:defines`
  Variable symbols declared with `defvar` for byte compilation.

When using `:bind` or `:bind-keymap` with `:map`, add the feature that defines
the keymap to `:after` if it is different from the package feature you are
configuring.

## Development

```shell
make check
```

## License

MIT
