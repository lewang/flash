# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

emacs-flash is an Emacs package implementing flash.nvim-style navigation — quick jumps using labels assigned to search matches. Key differentiator from avy: incremental input with automatic skipping of conflicting labels.

## Architecture

Six modules with clear separation:

```
emacs-flash.el           # Entry point, main loop, public API
emacs-flash-state.el     # Session state (pattern, matches, overlays)
emacs-flash-search.el    # Find matches in visible windows
emacs-flash-label.el     # Smart label assignment (skip conflicts)
emacs-flash-highlight.el # Overlays for backdrop, matches, labels
emacs-flash-jump.el      # Jump logic, window switching
emacs-flash-evil.el      # Evil-mode integration (post-MVP)
```

### Key Data Structures

```elisp
;; Session state
(cl-defstruct emacs-flash-state
  pattern matches windows overlays target start-window start-point)

;; Single match
(cl-defstruct emacs-flash-match
  pos end-pos label window fold)
```

### Core Flow

1. `emacs-flash-jump` creates state
2. Loop: search → label → highlight → read input
3. Input is either: label (jump), pattern char (extend search), or control (ESC/RET/backspace)
4. Cleanup overlays on exit

## Development

```bash
# Byte-compile all files
emacs -Q -batch -f batch-byte-compile *.el

# Run all tests
emacs -Q -batch -L . -L test -l ert -l emacs-flash-state -l emacs-flash-state-test -f ert-run-tests-batch-and-exit

# Load in running Emacs for testing
M-x load-file RET emacs-flash.el RET
```

**Rule**: Every module must have tests. All tests must pass before moving to next phase.

## Design Decisions

- **Overlays** for all visual feedback (not text properties)
- **Multi-window** search from the start
- **Evil-mode priority** but vanilla Emacs support
- **Smart labeling**: skip labels that could continue search pattern
- **Autojump**: jump immediately when single match
- **Prompt**: use minibuffer (not separate buffer)
- **Folded regions**: search inside folds, show one label per fold, unfold on jump
- Requires Emacs >= 27.1 (cl-defstruct, when-let)

## Planning Files

This project uses planning-with-files workflow:
- `task_plan.md` — phases and current status
- `architecture.md` — detailed module design
- `notes.md` — error log and decisions
- `examples.md` — test scenarios for all features
