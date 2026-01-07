# Notes: flash.nvim Analysis

## Sources

### Source 1: flash.nvim README
- URL: https://github.com/folke/flash.nvim
- Основные функции:
  - Search Integration — метки при использовании `/` или `?`
  - Инкрементальный ввод — можно набрать любое количество символов
  - Enhanced f/t/F/T — улучшенные motion команды
  - Treesitter Integration — работа с синтаксическими узлами
  - Multi-Window — поиск во всех окнах
  - Remote Actions — операции в других местах
  - Dot-repeatable — повтор через `.`

### Source 2: flash.nvim Source Code
- Path: `~/.local/share/nvim/lazy/flash.nvim/`
- Структура:
  ```
  lua/flash/
  ├── init.lua        # Точка входа
  ├── config.lua      # Конфигурация
  ├── state.lua       # Состояние сессии (ключевой файл!)
  ├── jump.lua        # Логика прыжка
  ├── labeler.lua     # Назначение меток
  ├── highlight.lua   # Отображение (extmarks)
  ├── prompt.lua      # Ввод пользователя
  ├── cache.lua       # Кэширование
  ├── search/         # Поиск совпадений
  └── plugins/        # char, search, treesitter режимы
  ```

### Source 3: avy (Emacs аналог)
- URL: https://github.com/abo-abo/avy
- GNU ELPA пакет
- Использует дерево решений
- Поддерживает символы, слова, строки, окна

## Synthesized Findings

### Архитектура flash.nvim

**State** — центральный объект сессии:
- `pattern` — текущий паттерн поиска
- `results` — список совпадений (matches)
- `wins` — окна для поиска
- `target` — текущее целевое совпадение
- `visible` — флаг видимости

**Match** — структура совпадения:
```lua
{
  pos = {line, col},      -- начальная позиция (1-indexed)
  end_pos = {line, col},  -- конечная позиция
  label = "a",            -- назначенная метка
  win = window_id,        -- окно
  fold = nil,             -- номер строки fold (если в fold)
}
```

**Labeler** — умный алгоритм:
1. Собирает доступные метки из `labels` строки
2. Пропускает метки, которые могут продолжить паттерн поиска
3. Сортирует совпадения по расстоянию от курсора
4. Назначает метки, переиспользуя их при изменении паттерна

**Step-loop** — основной цикл:
```lua
while state:step(opts) do end
state:hide()
```
Каждый step:
1. Читает символ
2. Проверяет, не метка ли это → jump
3. Обновляет паттерн
4. Проверяет autojump (если 1 результат)
5. Возвращает true для продолжения или nil для выхода

### Ключевые отличия от avy

| Аспект | flash.nvim | avy |
|--------|------------|-----|
| Ввод | Инкрементальный, любое кол-во символов | Обычно 1-2 символа |
| Конфликтные метки | Автоматически пропускает | Нет |
| Rainbow labels | Да | Нет |
| Treesitter | Встроенная поддержка | Нет |
| Remote actions | Да | Частично через avy-action |

### Техническая реализация для Emacs

**Overlays vs Text Properties:**
- Overlays — лучше для временных меток (легко удалить)
- `display` property — замена текста на метку
- `before-string` / `after-string` — метка рядом с текстом

**Пример overlay для метки:**
```elisp
(let ((ov (make-overlay pos (1+ pos))))
  (overlay-put ov 'display
    (propertize "a" 'face 'emacs-flash-label))
  (overlay-put ov 'emacs-flash t))
```

**Backdrop:**
```elisp
(let ((ov (make-overlay (window-start) (window-end))))
  (overlay-put ov 'face 'emacs-flash-backdrop))
```

**Ввод пользователя:**
```elisp
(while (not done)
  (let ((char (read-event)))
    (cond
     ((member char labels) (emacs-flash--jump char))
     ((eq char ?\C-g) (setq done t))
     (t (setq pattern (concat pattern (string char)))
        (emacs-flash--update-matches)))))
```

### Приоритеты для MVP

1. Базовый jump с инкрементальным вводом
2. Подсветка совпадений
3. Метки с умным назначением (пропуск конфликтных)
4. Backdrop эффект
5. Autojump при единственном совпадении

---

## Log

### Phase 2 — Решение по folded regions

**What happened:**
Изучили как flash.nvim обрабатывает свёрнутые регионы (folds).

**Решение flash.nvim:**
1. Ищет совпадения внутри folds
2. Показывает только одну метку на fold (первую)
3. Метка отображается на строке fold, не внутри
4. При прыжке fold разворачивается (`zO`)

**Ключевой код:**
```lua
-- state.lua — определяет fold для match
local fold = vim.fn.foldclosed(match.pos[1])
match.fold = fold ~= -1 and fold or nil

-- labeler.lua — пропуск остальных matches в том же fold
if folds[match.fold] then skip = true end

-- highlight.lua — метка на строке fold
if match.fold then row = match.fold - 1 end

-- jump.lua — разворачивание
if vim.fn.foldclosed(line) ~= -1 then vim.cmd("normal! zO") end
```

**Для Emacs:**
- `(invisible-p pos)` — проверка invisible text
- `(outline-show-entry)` или `(org-show-context)` — разворачивание
- Добавить поле `fold` в `emacs-flash-match`

### Phase 3.4 — emacs-flash-label.el

**Implemented:**
- `emacs-flash-labels` — дефолтная строка меток "asdfjkl;..."
- `emacs-flash-label-matches` — назначение меток совпадениям
- `emacs-flash--available-labels` — пропуск конфликтных меток
- `emacs-flash--label-conflicts-p` — проверка конфликта через поиск
- `emacs-flash--sort-by-distance` — сортировка по расстоянию от курсора
- `emacs-flash-find-match-by-label` — поиск match по метке

**Design decisions:**
- Labels assigned to matches closest to cursor first
- Conflict check: if "ab" found and user could type "abc", label 'c' skipped
- Search in all windows for conflict detection

### Phase 3.5 — emacs-flash-jump.el

**Implemented:**
- `emacs-flash-jump-to-match` — прыжок к match с переключением окна
- `emacs-flash-jump-to-label` — прыжок по метке
- `emacs-flash-jump-to-first` — прыжок к первому match
- `emacs-flash-return-to-start` — возврат к исходной позиции
- `emacs-flash--unfold-at-point` — разворачивание fold

**Fold support:**
- org-mode: `org-show-context`
- outline-mode: `outline-show-entry`
- hideshow: `hs-show-block`
- fallback: remove invisible property

### Phase 3.6 — emacs-flash.el

**Implemented:**
- `emacs-flash-jump` — главная интерактивная команда
- `emacs-flash--loop` — основной цикл ввода
- `emacs-flash--format-prompt` — форматирование промпта

**Defcustoms:**
- `emacs-flash-labels` — символы для меток
- `emacs-flash-multi-window` — поиск во всех окнах
- `emacs-flash-autojump` — прыжок при единственном совпадении
- `emacs-flash-backdrop` — затемнение фона
- `emacs-flash-case-fold` — игнорирование регистра

**Input handling:**
- ESC — отмена, возврат к исходной позиции
- RET — прыжок к первому совпадению
- Backspace — удаление последнего символа
- Label char — прыжок к метке
- Other char — добавление к паттерну
