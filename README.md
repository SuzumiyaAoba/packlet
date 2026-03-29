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
  `:hook`, `:hook-setq`, `:hook-call`, `:hook-add`, `:hook-enable`, `:bind`,
  `:bind-keymap`, `:prefix-map`, `:enable`, `:faces`, `:advice`,
  `:mode`, `:remap`, `:derived-mode`, `:interpreter`, `:magic`,
  `:magic-fallback`, `:after-load`, `:idle`, and `:demand` registrations
  from that source instead of stacking duplicates.
- File-backed reevaluation is transactional. If the new evaluation fails part
  way through, `packlet` restores the previously working registrations.
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

## Keywords

- `:file`
  Override the library name used for autoloads, declarations, and demand
  loading. By default this is the same as the feature symbol.
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
  Forms evaluated once after the feature and every `:after` dependency are
  loaded.
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
  You can also use `(some-hook some-function delay)` to run the function from
  an idle timer after the hook fires. List-style entries additionally accept
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
  supported.
- `:hook-add`
  `(some-hook target-hook function)` entries that add `function` to
  `target-hook` when `some-hook` runs. `function` must be a symbol.
  List-style entries additionally accept `:append t` and `:local t`, for
  example `(go-mode-hook before-save-hook gofmt :local t)`.
- `:hook-enable`
  `(some-hook function)` or `(some-hook function arg)` entries that call a
  mode-like function from a hook. This is useful for patterns such as
  `(prog-mode-hook display-line-numbers-mode)`.
- `:bind`
  Global key bindings such as `("C-c p" . some-command)` or keymap groups such
  as `(:map some-mode-map ("C-c p" . some-command))`.
- `:bind-keymap`
  Lazy prefix-key bindings such as `("C-c p" . projectile-command-map)` or
  keymap groups such as `(:map some-mode-map ("C-c p" . some-prefix-map))`.
  The first key press loads the feature, swaps in the real keymap, and replays
  the original key sequence.
- `:prefix-map`
  Symbols naming sparse keymaps that should be created when still unbound.
  This is useful together with `:bind-keymap` and `:bind (:map ...)` when a
  package uses a config-owned prefix map.
- `:enable`
  Symbols naming mode-like functions to call after the feature and every
  `:after` dependency are loaded.  A bare symbol calls the function with `1`.
  A tuple `(function arg)` calls it with `arg`, for example
  `(treemacs-git-mode 'deferred)`.
- `:faces`
  `(face :attribute value ...)` entries applied after the feature and every
  `:after` dependency are loaded.  `:copy` copies another face before applying
  explicit attributes, for example
  `(anzu-mode-line :copy mode-line)` or
  `(git-gutter:modified :background "purple")`.
- `:advice`
  `(target how function)` entries added with `advice-add`.
  List-style entries additionally accept `:name` and `:depth`, for example
  `(projectile-find-file :override consult-projectile-find-file :depth -10)`.
- `:after`
  Feature symbols that must be loaded before `:config` or `:demand` becomes
  active.
- `:after-load`
  `(feature body...)` forms evaluated after an arbitrary feature loads, even if
  it is not the package feature being configured.
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
