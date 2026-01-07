# Task Plan: emacs-flash

## Goal
Создать пакет для Emacs, реализующий функциональность flash.nvim — быструю навигацию по коду с метками (labels).

## Phases

- [x] Phase 1: Исследование
  - [x] 1.1 Изучить исходники flash.nvim (state, labeler, highlight, jump)
  - [x] 1.2 Изучить avy и другие аналоги в Emacs
  - [x] 1.3 Сравнительный анализ, определить преимущества flash.nvim

- [x] Phase 2: Проектирование
  - [x] 2.1 Определить scope MVP (только jump, multi-window, autojump)
  - [x] 2.2 Спроектировать модульную структуру
  - [x] 2.3 Определить структуры данных (state, match)
  - [x] 2.4 Описать основной цикл (search → label → highlight → input)

- [ ] Phase 3: Реализация MVP
  - [x] 3.1 emacs-flash-state.el — cl-defstruct для state и match
  - [ ] 3.2 emacs-flash-search.el — поиск совпадений в видимых окнах
  - [ ] 3.3 emacs-flash-highlight.el — overlays для backdrop, matches, labels
  - [ ] 3.4 emacs-flash-label.el — назначение меток с пропуском конфликтных
  - [ ] 3.5 emacs-flash-jump.el — прыжок к метке, переключение окон
  - [ ] 3.6 emacs-flash.el — главный модуль, основной цикл, defcustom
  - [ ] 3.7 Ручное тестирование базового функционала

- [ ] Phase 4: Расширенные функции
  - [ ] 4.1 emacs-flash-evil.el — интеграция с evil-mode
  - [ ] 4.2 Rainbow labels (разноцветные метки)
  - [ ] 4.3 Поддержка regex поиска
  - [ ] 4.4 Case-fold настройка

- [ ] Phase 5: Тестирование и документация
  - [ ] 5.1 ERT тесты для основных функций
  - [ ] 5.2 README.md с примерами использования
  - [ ] 5.3 Подготовка к публикации в MELPA

## Blocked / Open Questions
(все решены)

## Decisions Made
- [Режим]: evil-mode — приоритет интеграции с evil
- [MVP scope]: Только базовый jump с инкрементальным вводом и метками
- [Multi-window]: Да, поддержка всех видимых окон с самого начала
- [Overlays]: Использовать overlays для всего отображения (не text-properties)
- [Требования]: Emacs >= 27.1 (cl-defstruct, when-let)
- [Prompt]: Использовать minibuffer (не отдельный buffer)
- [Folded regions]: Как в flash.nvim — искать внутри folds, показывать одну метку на fold, разворачивать при прыжке
- [Тесты]: ERT тесты для каждого модуля, все тесты должны проходить

## Status
**Phase 3.2** — Реализация emacs-flash-search.el

## Files
- `task_plan.md` — этот файл
- `architecture.md` — детальная архитектура пакета
- `notes.md` — история: ошибки, решения, находки
- `examples.md` — сценарии тестирования всех фич
- `CLAUDE.md` — инструкции для Claude Code
