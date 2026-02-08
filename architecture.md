# flash: Архитектура

## Структура файлов

```
flash/
├── flash.el           # Главный файл, точка входа
├── flash-state.el     # Состояние сессии
├── flash-search.el    # Поиск совпадений
├── flash-label.el     # Назначение меток
├── flash-highlight.el # Отображение (overlays)
├── flash-jump.el      # Логика прыжка
└── flash-evil.el      # Интеграция с evil-mode (опционально)
```

## Модули

### 1. flash.el — Главный модуль

```elisp
;;; flash.el --- Flash-style navigation for Emacs -*- lexical-binding: t -*-

;; Публичные команды:
;; - `flash-jump` — основная команда навигации

;; Customization group:
;; - `flash-labels` — строка с метками
;; - `flash-multi-window` — искать во всех окнах
;; - `flash-autojump` — прыгать при единственном совпадении
;; - `flash-backdrop` — затемнять фон

(require 'flash-state)
(require 'flash-search)
(require 'flash-label)
(require 'flash-highlight)
(require 'flash-jump)

;;;###autoload
(defun flash-jump ()
  "Start flash jump session."
  (interactive)
  (let ((state (flash-state-create)))
    (unwind-protect
        (flash--loop state)
      (flash-state-cleanup state))))
```

### 2. flash-state.el — Состояние

```elisp
(cl-defstruct flash-state
  pattern           ; текущий паттерн поиска (string)
  matches           ; список совпадений (list of flash-match)
  windows           ; список окон для поиска
  overlays          ; все созданные overlays
  target            ; текущее целевое совпадение
  start-window      ; исходное окно
  start-point)      ; исходная позиция

(cl-defstruct flash-match
  pos               ; позиция начала (marker)
  end-pos           ; позиция конца (marker)
  label             ; назначенная метка (char или nil)
  window            ; окно
  fold)             ; начало fold-региона (или nil если не в fold)

(defun flash-state-create ()
  "Create new flash state."
  (make-flash-state
   :pattern ""
   :matches nil
   :windows (if flash-multi-window
                (window-list)
              (list (selected-window)))
   :overlays nil
   :target nil
   :start-window (selected-window)
   :start-point (point)))

(defun flash-state-cleanup (state)
  "Clean up STATE, remove all overlays."
  (mapc #'delete-overlay (flash-state-overlays state)))
```

### 3. flash-search.el — Поиск

```elisp
(defun flash-search (state)
  "Find all matches for STATE pattern in all windows."
  (let ((pattern (flash-state-pattern state))
        (windows (flash-state-windows state))
        matches)
    (when (> (length pattern) 0)
      (dolist (win windows)
        (with-selected-window win
          (save-excursion
            (goto-char (window-start))
            (let ((limit (window-end nil t)))
              (while (search-forward pattern limit t)
                (push (make-flash-match
                       :pos (copy-marker (match-beginning 0))
                       :end-pos (copy-marker (match-end 0))
                       :label nil
                       :window win)
                      matches)))))))
    (setf (flash-state-matches state) (nreverse matches))))
```

### 4. flash-label.el — Назначение меток

```elisp
(defcustom flash-labels "asdfjkl;ghqwertyuiopzxcvbnm"
  "Characters to use as jump labels."
  :type 'string
  :group 'flash)

(defun flash-label-matches (state)
  "Assign labels to matches in STATE."
  (let* ((matches (flash-state-matches state))
         (pattern (flash-state-pattern state))
         (labels (flash--available-labels pattern))
         (sorted (flash--sort-by-distance matches)))
    (cl-loop for match in sorted
             for label in labels
             do (setf (flash-match-label match) label))))

(defun flash--available-labels (pattern)
  "Return labels that won't conflict with PATTERN continuation."
  (let ((chars (string-to-list flash-labels)))
    (if (string-empty-p pattern)
        chars
      ;; Пропускаем метки, которые могут продолжить паттерн
      (cl-remove-if
       (lambda (char)
         (flash--label-conflicts-p pattern char))
       chars))))

(defun flash--label-conflicts-p (pattern char)
  "Check if CHAR as next input would match existing text after PATTERN."
  (let ((extended (concat pattern (char-to-string char))))
    (save-excursion
      (goto-char (point-min))
      (search-forward extended nil t))))

(defun flash--sort-by-distance (matches)
  "Sort MATCHES by distance from current point."
  (let ((pos (point)))
    (sort matches
          (lambda (a b)
            (< (abs (- (marker-position (flash-match-pos a)) pos))
               (abs (- (marker-position (flash-match-pos b)) pos)))))))
```

### 5. flash-highlight.el — Отображение

```elisp
(defface flash-label
  '((t :foreground "white" :background "#ff007c" :weight bold))
  "Face for jump labels."
  :group 'flash)

(defface flash-match
  '((t :background "#3e68d7" :foreground "white"))
  "Face for search matches."
  :group 'flash)

(defface flash-backdrop
  '((t :foreground "gray40"))
  "Face for backdrop effect."
  :group 'flash)

(defun flash-highlight-update (state)
  "Update highlighting for STATE."
  ;; Удаляем старые overlays
  (mapc #'delete-overlay (flash-state-overlays state))
  (setf (flash-state-overlays state) nil)

  ;; Backdrop
  (when flash-backdrop
    (dolist (win (flash-state-windows state))
      (with-selected-window win
        (let ((ov (make-overlay (window-start) (window-end nil t))))
          (overlay-put ov 'face 'flash-backdrop)
          (overlay-put ov 'flash t)
          (push ov (flash-state-overlays state))))))

  ;; Matches и labels
  (dolist (match (flash-state-matches state))
    (let* ((pos (flash-match-pos match))
           (end (flash-match-end-pos match))
           (label (flash-match-label match)))
      ;; Match highlight
      (let ((ov (make-overlay pos end)))
        (overlay-put ov 'face 'flash-match)
        (overlay-put ov 'flash t)
        (overlay-put ov 'priority 100)
        (push ov (flash-state-overlays state)))
      ;; Label
      (when label
        (let ((ov (make-overlay end (1+ end))))
          (overlay-put ov 'before-string
                       (propertize (char-to-string label)
                                   'face 'flash-label))
          (overlay-put ov 'flash t)
          (overlay-put ov 'priority 200)
          (push ov (flash-state-overlays state)))))))
```

### 6. flash-jump.el — Логика прыжка

```elisp
(defun flash-jump-to (state label)
  "Jump to match with LABEL in STATE."
  (when-let ((match (cl-find label
                             (flash-state-matches state)
                             :key #'flash-match-label)))
    (let ((win (flash-match-window match))
          (pos (flash-match-pos match)))
      ;; Переключаем окно если нужно
      (unless (eq win (selected-window))
        (select-window win))
      ;; Прыгаем
      (goto-char pos)
      t)))
```

### 7. Основной цикл

```elisp
(defun flash--loop (state)
  "Main input loop for STATE."
  (catch 'done
    (while t
      ;; Обновляем поиск и отображение
      (flash-search state)
      (flash-label-matches state)
      (flash-highlight-update state)

      ;; Autojump если одно совпадение
      (when (and flash-autojump
                 (= (length (flash-state-matches state)) 1))
        (flash-jump-to state
          (flash-match-label (car (flash-state-matches state))))
        (throw 'done t))

      ;; Показываем prompt и читаем ввод
      (let* ((prompt (format "Flash [%s]: "
                             (flash-state-pattern state)))
             (char (read-char prompt)))
        (cond
         ;; Escape — отмена
         ((= char ?\e)
          (goto-char (flash-state-start-point state))
          (select-window (flash-state-start-window state))
          (throw 'done nil))

         ;; Enter — прыжок к первому совпадению
         ((= char ?\r)
          (when (flash-state-matches state)
            (flash-jump-to state
              (flash-match-label (car (flash-state-matches state)))))
          (throw 'done t))

         ;; Backspace — удалить символ
         ((= char ?\C-?)
          (let ((pattern (flash-state-pattern state)))
            (when (> (length pattern) 0)
              (setf (flash-state-pattern state)
                    (substring pattern 0 -1)))))

         ;; Проверяем, не метка ли это
         ((flash-jump-to state char)
          (throw 'done t))

         ;; Добавляем символ к паттерну
         (t
          (setf (flash-state-pattern state)
                (concat (flash-state-pattern state)
                        (char-to-string char)))))))))
```

## Порядок реализации

1. **flash-state.el** — структуры данных
2. **flash-search.el** — поиск совпадений
3. **flash-highlight.el** — отображение
4. **flash-label.el** — назначение меток
5. **flash-jump.el** — прыжок
6. **flash.el** — главный модуль и цикл
7. Тестирование базового функционала
8. **flash-evil.el** — интеграция с evil (после MVP)

## Зависимости

- Emacs >= 27.1 (для `cl-defstruct`, `when-let`)
- `cl-lib` — встроенная библиотека

## Конфигурация по умолчанию

```elisp
(defcustom flash-labels "asdfjkl;ghqwertyuiopzxcvbnm"
  "Characters to use as jump labels, ordered by priority.")

(defcustom flash-multi-window t
  "Search in all visible windows.")

(defcustom flash-autojump t
  "Automatically jump when there is only one match.")

(defcustom flash-backdrop t
  "Show backdrop effect to dim non-matching text.")

(defcustom flash-case-fold t
  "Ignore case when searching.")
```
