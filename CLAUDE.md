# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

packlet is an Emacs Lisp DSL for lazy-first package configuration. It describes *when* and *how* configuration becomes active — it does **not** install packages. Requires Emacs 29.1+.

## Build & Test Commands

```sh
make compile          # Byte-compile packlet.el and test file
make test             # Run ERT test suite in batch mode
make check            # Compile + test (run before PR)
make check EMACS=/path/to/emacs  # Override Emacs binary
```

## Architecture

Single-file implementation in `packlet.el` (~2500 lines) with `lexical-binding: t`.

### Core Macro

`packlet` (the main DSL macro) takes a FEATURE symbol and a keyword-based body. It parses the body into sections, expands to registration and configuration forms, and supports transactional reevaluation without stacking duplicate handlers.

### Internal Organization

- **Parsing & normalization** (`packlet--parse-body`, `packlet--normalize-*`): Convert DSL keyword bodies into structured data. Each keyword type (hooks, bindings, modes, advice, etc.) has its own normalizer.
- **Source scope management** (`packlet--*-source-scope*`, `packlet--source-entry*`): Track which file or buffer "owns" each registration. Enables transactional reevaluation — re-evaluating a file replaces old registrations rather than stacking.
- **Autoload management** (`packlet--maybe-autoload`): Prefers package autoload definitions; falls back to feature file.
- **After-load / idle / demand loading**: Deferred evaluation tied to feature loading, idle time, or explicit demand triggers.
- **Feature inspection** (`packlet-describe-source`, `packlet-describe-feature`, `packlet-explain-feature`, `packlet-list-features`): Interactive commands for debugging registrations and runtime state.

### Naming Conventions

- Public API: short names (e.g., `packlet`, `packlet-describe-source`)
- Internal helpers: `packlet--` prefix
- Test helpers/cases: `packlet-test-` prefix

## Testing

Tests live in `test/packlet-test.el` using ERT. Tests generate temporary features at runtime rather than relying on committed fixture files. Key test areas: parsing, autoload registration, hook lifecycle, transactional reevaluation (no duplicate/stale handlers), and feature inspection.

## Commit Style

Conventional Commits (lowercase): `feat:`, `fix:`, `refactor:`, `docs:`, etc. One focused change per commit. When changing DSL semantics, include before/after Lisp snippets in the PR description.
