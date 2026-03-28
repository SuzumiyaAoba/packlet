# packlet

`packlet` is a small DSL for lazy-first Emacs package setup.

It focuses on the common parts of package configuration:

- autoloading interactive commands
- autoloading helper functions
- registering `auto-mode-alist` entries
- registering `interpreter-mode-alist` entries
- registering `magic-mode-alist` entries
- registering `magic-fallback-mode-alist` entries
- wiring hooks, global key bindings, and package keymaps
- wiring lazy prefix-key keymaps
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
  `:setq`, `:custom`, `:config`, `:hook`, `:bind`, `:bind-keymap`, `:mode`,
  `:interpreter`, `:magic`, `:magic-fallback`, `:after-load`, `:idle`, and
  `:demand`
  registrations from that source instead of stacking duplicates.
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
  an idle timer after the hook fires.
- `:bind`
  Global key bindings such as `("C-c p" . some-command)` or keymap groups such
  as `(:map some-mode-map ("C-c p" . some-command))`.
- `:bind-keymap`
  Lazy prefix-key bindings such as `("C-c p" . projectile-command-map)` or
  keymap groups such as `(:map some-mode-map ("C-c p" . some-prefix-map))`.
  The first key press loads the feature, swaps in the real keymap, and replays
  the original key sequence.
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
