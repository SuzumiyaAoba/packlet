# packlet

`packlet` is a small DSL for lazy-first Emacs package setup.

It focuses on the common parts of package configuration:

- autoloading interactive commands
- registering `auto-mode-alist` entries
- wiring hooks and global key bindings
- deferring configuration until a feature is actually loaded
- optionally demanding a feature once its dependencies are ready

It does not install packages by itself. `packlet` only describes when and how
configuration should become active.

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
  :bind ("C-x g" . magit-status)
  :after project
  :config
  (setq magit-display-buffer-function
        #'magit-display-buffer-same-window-except-diff-v1))
```

## Keywords

- `:init`
  Forms evaluated immediately.
- `:config`
  Forms evaluated once after the feature and every `:after` dependency are
  loaded.
- `:commands`
  Symbols to autoload from the package file.
- `:mode`
  `("\\\\.ext\\\\'" . some-mode)` pairs added to `auto-mode-alist`.
- `:hook`
  `(some-hook . some-function)` pairs added with `add-hook`.
- `:bind`
  Global key bindings such as `("C-c p" . some-command)`.
- `:after`
  Feature symbols that must be loaded before `:config` or `:demand` becomes
  active.
- `:demand`
  Require the feature once its `:after` dependencies are satisfied. With no
  value, this defaults to `t`. You can also pass a condition form.

## Development

```shell
make check
```

## License

MIT
