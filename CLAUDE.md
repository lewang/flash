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
emacs-flash-evil.el      # Evil-mode integration
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
# Run all tests
emacs -Q -batch -L . -L test -l ert \
  -l emacs-flash-state-test.el \
  -l emacs-flash-search-test.el \
  -l emacs-flash -l emacs-flash-highlight-test.el \
  -l emacs-flash-label-test.el \
  -l emacs-flash-jump-test.el \
  -l emacs-flash-test.el \
  -f ert-run-tests-batch-and-exit

# Load in running Emacs for testing
M-x load-file RET emacs-flash.el RET
```

**Rule**: Every module must have tests. All tests must pass before moving to next phase.

## Working Style

- **Always make changes yourself** — don't ask the user to edit code
- **Always run and test code yourself** — use batch Emacs to run tests
- **Only ask user to verify** when you absolutely cannot test it yourself (e.g., interactive UI behavior)

## Design Decisions

- **Overlays** for all visual feedback (not text properties)
- **Multi-window** search from the start
- **Evil-mode priority** but vanilla Emacs support
- **Smart labeling**: skip labels that could continue search pattern
- **Autojump**: jump immediately when single match
- **Prompt**: use minibuffer (not separate buffer)
- **Folded regions**: search inside folds, show one label per fold, unfold on jump
- **Literal search only**: no regex (keep it simple, literal search is enough for navigation)
- Requires Emacs >= 27.1 (cl-defstruct, when-let)

## flash.nvim Reference

### Highlight Groups (for face design)
- `FlashMatch` → links to `Search`
- `FlashLabel` → links to `Substitute`
- Uses standard highlight groups for theme compatibility

### Labeler Algorithm
1. Collect available labels from `labels` string
2. Skip labels that could continue search pattern
3. Sort matches by distance from cursor
4. Assign labels, reusing them when pattern changes

### Fold Handling
```lua
-- state.lua — detect fold for match
local fold = vim.fn.foldclosed(match.pos[1])
match.fold = fold ~= -1 and fold or nil

-- labeler.lua — skip other matches in same fold
if folds[match.fold] then skip = true end

-- highlight.lua — show label on fold line
if match.fold then row = match.fold - 1 end

-- jump.lua — unfold on jump
if vim.fn.foldclosed(line) ~= -1 then vim.cmd("normal! zO") end
```

---

## Error Log

### Phase 4.2 — Rainbow Labels: яркие цвета мешали читать

**Problem:** Яркие цвета меток (inverse-video, bold) мешали читать следующий символ.

**Solution:** Разные настройки для светлой/тёмной темы:
```elisp
(defface emacs-flash-label-red
  '((((background dark))
     :inherit font-lock-warning-face :inverse-video t)
    (((background light))
     :inherit (highlight font-lock-warning-face)))
  ...)
```
- Для dark theme: `:inverse-video t` (цвет текста становится фоном)
- Для light theme: `:inherit (highlight font-lock-*-face)` (мягкий фон)

### Phase 4.2 — Опция отключения подсветки совпадений

**Problem:** Пользователь хотел видеть оригинальный синтаксис, без оранжевой подсветки "def".

**Solution:** Добавлен `emacs-flash-highlight-matches` (defcustom, default t).
Когда nil — только метки показываются, совпадения не подсвечиваются.

### evil + undo-tree: "Enable global-undo-tree-mode"

**Problem:**
```
evil--check-undo-system: Enable 'global-undo-tree-mode' to use undo-tree commands.
```

**Cause:** Evil проверяет `undo-tree-mode` (буферный), не глобальный. В non-file буферах (*scratch*) undo-tree-mode не был включен.

**Solution:**
```elisp
(add-hook 'evil-local-mode-hook 'turn-on-undo-tree-mode)
```

**Also important:**
- `evil-undo-system` должен быть задан в `early-init.el` ДО загрузки evil
- Порядок в init.el: undo-tree → evil → evil-collection
- Пакеты с `:after evil` должны быть ПОСЛЕ определения evil в init.el

---

## Planning Files

- `task_plan.md` — phases and current status
- `architecture.md` — detailed module design
- `examples.md` — test scenarios for all features
