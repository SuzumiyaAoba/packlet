# Repository Guidelines

## Project Structure & Module Organization
`packlet.el` contains the public `packlet` macro and its internal helpers. Tests live in `test/packlet-test.el` and use temporary generated features instead of committed fixtures. `README.md` is the user-facing reference and should stay aligned with behavior changes. `Makefile` defines the standard development entry points. Byte-compiled artifacts such as `*.elc` are generated output and should not be edited by hand.

## Build, Test, and Development Commands
Use the `Makefile` targets so local checks match CI-style batch execution:

- `make compile`: byte-compile `packlet.el` and the test file with `emacs -Q --batch`.
- `make test`: run the ERT suite in batch mode.
- `make check`: run compile and test together; use this before opening a PR.

If needed, override the Emacs binary explicitly, for example: `make check EMACS=/Applications/Emacs.app/Contents/MacOS/Emacs`.

## Coding Style & Naming Conventions
Write Emacs Lisp with `lexical-binding: t` headers. Follow standard `emacs-lisp-mode` indentation and keep forms compact and readable rather than heavily nested. Public API names should stay short and user-facing, while internal helpers should use the `packlet--...` prefix. Test helpers and test cases should use the `packlet-test-...` prefix. Add docstrings to functions and macros, and keep comments limited to non-obvious behavior.

## Testing Guidelines
Add tests with `ert-deftest`, preferably in `test/packlet-test.el` unless a new `test/*-test.el` file makes the suite clearer. Cover both expected behavior and edge cases when changing parsing, autoload registration, hooks, `:after`, or `:demand`. Run `make test` during development and `make check` before submission.

## Commit & Pull Request Guidelines
The current history uses Conventional Commit style (`feat: initial packlet package`); follow the same lowercase pattern, for example `fix: avoid duplicate config`. Keep each commit focused on one change. Pull requests should include a short summary, linked issue if applicable, updated documentation when behavior changes, and the commands you ran to validate the patch. Include before/after Emacs Lisp snippets when changing DSL semantics.

## Compatibility Notes
Target Emacs 29.1 or newer, matching `Package-Requires`. When introducing new behavior, prefer additions that preserve the lazy-first design described in `README.md`.
