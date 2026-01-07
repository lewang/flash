# emacs-flash: Архитектура

## Структура файлов

```
emacs-flash/
├── emacs-flash.el           # Главный файл, точка входа
├── emacs-flash-state.el     # Состояние сессии
├── emacs-flash-search.el    # Поиск совпадений
├── emacs-flash-label.el     # Назначение меток
├── emacs-flash-highlight.el # Отображение (overlays)
├── emacs-flash-jump.el      # Логика прыжка
└── emacs-flash-evil.el      # Интеграция с evil-mode (опционально)
```

## Модули

### 1. emacs-flash.el — Главный модуль

```elisp
;;; emacs-flash.el --- Flash-style navigation for Emacs -*- lexical-binding: t -*-

;; Публичные команды:
;; - `emacs-flash-jump` — основная команда навигации

;; Customization group:
;; - `emacs-flash-labels` — строка с метками
;; - `emacs-flash-multi-window` — искать во всех окнах
;; - `emacs-flash-autojump` — прыгать при единственном совпадении
;; - `emacs-flash-backdrop` — затемнять фон

(require 'emacs-flash-state)
(require 'emacs-flash-search)
(require 'emacs-flash-label)
(require 'emacs-flash-highlight)
(require 'emacs-flash-jump)

;;;###autoload
(defun emacs-flash-jump ()
  "Start flash jump session."
  (interactive)
  (let ((state (emacs-flash-state-create)))
    (unwind-protect
        (emacs-flash--loop state)
      (emacs-flash-state-cleanup state))))
```

### 2. emacs-flash-state.el — Состояние

```elisp
(cl-defstruct emacs-flash-state
  pattern           ; текущий паттерн поиска (string)
  matches           ; список совпадений (list of emacs-flash-match)
  windows           ; список окон для поиска
  overlays          ; все созданные overlays
  target            ; текущее целевое совпадение
  start-window      ; исходное окно
  start-point)      ; исходная позиция

(cl-defstruct emacs-flash-match
  pos               ; позиция начала (marker)
  end-pos           ; позиция конца (marker)
  label             ; назначенная метка (char или nil)
  window            ; окно
  fold)             ; начало fold-региона (или nil если не в fold)

(defun emacs-flash-state-create ()
  "Create new flash state."
  (make-emacs-flash-state
   :pattern ""
   :matches nil
   :windows (if emacs-flash-multi-window
                (window-list)
              (list (selected-window)))
   :overlays nil
   :target nil
   :start-window (selected-window)
   :start-point (point)))

(defun emacs-flash-state-cleanup (state)
  "Clean up STATE, remove all overlays."
  (mapc #'delete-overlay (emacs-flash-state-overlays state)))
```

### 3. emacs-flash-search.el — Поиск

```elisp
(defun emacs-flash-search (state)
  "Find all matches for STATE pattern in all windows."
  (let ((pattern (emacs-flash-state-pattern state))
        (windows (emacs-flash-state-windows state))
        matches)
    (when (> (length pattern) 0)
      (dolist (win windows)
        (with-selected-window win
          (save-excursion
            (goto-char (window-start))
            (let ((limit (window-end nil t)))
              (while (search-forward pattern limit t)
                (push (make-emacs-flash-match
                       :pos (copy-marker (match-beginning 0))
                       :end-pos (copy-marker (match-end 0))
                       :label nil
                       :window win)
                      matches)))))))
    (setf (emacs-flash-state-matches state) (nreverse matches))))
```

### 4. emacs-flash-label.el — Назначение меток

```elisp
(defcustom emacs-flash-labels "asdfjkl;ghqwertyuiopzxcvbnm"
  "Characters to use as jump labels."
  :type 'string
  :group 'emacs-flash)

(defun emacs-flash-label-matches (state)
  "Assign labels to matches in STATE."
  (let* ((matches (emacs-flash-state-matches state))
         (pattern (emacs-flash-state-pattern state))
         (labels (emacs-flash--available-labels pattern))
         (sorted (emacs-flash--sort-by-distance matches)))
    (cl-loop for match in sorted
             for label in labels
             do (setf (emacs-flash-match-label match) label))))

(defun emacs-flash--available-labels (pattern)
  "Return labels that won't conflict with PATTERN continuation."
  (let ((chars (string-to-list emacs-flash-labels)))
    (if (string-empty-p pattern)
        chars
      ;; Пропускаем метки, которые могут продолжить паттерн
      (cl-remove-if
       (lambda (char)
         (emacs-flash--label-conflicts-p pattern char))
       chars))))

(defun emacs-flash--label-conflicts-p (pattern char)
  "Check if CHAR as next input would match existing text after PATTERN."
  (let ((extended (concat pattern (char-to-string char))))
    (save-excursion
      (goto-char (point-min))
      (search-forward extended nil t))))

(defun emacs-flash--sort-by-distance (matches)
  "Sort MATCHES by distance from current point."
  (let ((pos (point)))
    (sort matches
          (lambda (a b)
            (< (abs (- (marker-position (emacs-flash-match-pos a)) pos))
               (abs (- (marker-position (emacs-flash-match-pos b)) pos)))))))
```

### 5. emacs-flash-highlight.el — Отображение

```elisp
(defface emacs-flash-label
  '((t :foreground "white" :background "#ff007c" :weight bold))
  "Face for jump labels."
  :group 'emacs-flash)

(defface emacs-flash-match
  '((t :background "#3e68d7" :foreground "white"))
  "Face for search matches."
  :group 'emacs-flash)

(defface emacs-flash-backdrop
  '((t :foreground "gray40"))
  "Face for backdrop effect."
  :group 'emacs-flash)

(defun emacs-flash-highlight-update (state)
  "Update highlighting for STATE."
  ;; Удаляем старые overlays
  (mapc #'delete-overlay (emacs-flash-state-overlays state))
  (setf (emacs-flash-state-overlays state) nil)

  ;; Backdrop
  (when emacs-flash-backdrop
    (dolist (win (emacs-flash-state-windows state))
      (with-selected-window win
        (let ((ov (make-overlay (window-start) (window-end nil t))))
          (overlay-put ov 'face 'emacs-flash-backdrop)
          (overlay-put ov 'emacs-flash t)
          (push ov (emacs-flash-state-overlays state))))))

  ;; Matches и labels
  (dolist (match (emacs-flash-state-matches state))
    (let* ((pos (emacs-flash-match-pos match))
           (end (emacs-flash-match-end-pos match))
           (label (emacs-flash-match-label match)))
      ;; Match highlight
      (let ((ov (make-overlay pos end)))
        (overlay-put ov 'face 'emacs-flash-match)
        (overlay-put ov 'emacs-flash t)
        (overlay-put ov 'priority 100)
        (push ov (emacs-flash-state-overlays state)))
      ;; Label
      (when label
        (let ((ov (make-overlay end (1+ end))))
          (overlay-put ov 'before-string
                       (propertize (char-to-string label)
                                   'face 'emacs-flash-label))
          (overlay-put ov 'emacs-flash t)
          (overlay-put ov 'priority 200)
          (push ov (emacs-flash-state-overlays state)))))))
```

### 6. emacs-flash-jump.el — Логика прыжка

```elisp
(defun emacs-flash-jump-to (state label)
  "Jump to match with LABEL in STATE."
  (when-let ((match (cl-find label
                             (emacs-flash-state-matches state)
                             :key #'emacs-flash-match-label)))
    (let ((win (emacs-flash-match-window match))
          (pos (emacs-flash-match-pos match)))
      ;; Переключаем окно если нужно
      (unless (eq win (selected-window))
        (select-window win))
      ;; Прыгаем
      (goto-char pos)
      t)))
```

### 7. Основной цикл

```elisp
(defun emacs-flash--loop (state)
  "Main input loop for STATE."
  (catch 'done
    (while t
      ;; Обновляем поиск и отображение
      (emacs-flash-search state)
      (emacs-flash-label-matches state)
      (emacs-flash-highlight-update state)

      ;; Autojump если одно совпадение
      (when (and emacs-flash-autojump
                 (= (length (emacs-flash-state-matches state)) 1))
        (emacs-flash-jump-to state
          (emacs-flash-match-label (car (emacs-flash-state-matches state))))
        (throw 'done t))

      ;; Показываем prompt и читаем ввод
      (let* ((prompt (format "Flash [%s]: "
                             (emacs-flash-state-pattern state)))
             (char (read-char prompt)))
        (cond
         ;; Escape — отмена
         ((= char ?\e)
          (goto-char (emacs-flash-state-start-point state))
          (select-window (emacs-flash-state-start-window state))
          (throw 'done nil))

         ;; Enter — прыжок к первому совпадению
         ((= char ?\r)
          (when (emacs-flash-state-matches state)
            (emacs-flash-jump-to state
              (emacs-flash-match-label (car (emacs-flash-state-matches state)))))
          (throw 'done t))

         ;; Backspace — удалить символ
         ((= char ?\C-?)
          (let ((pattern (emacs-flash-state-pattern state)))
            (when (> (length pattern) 0)
              (setf (emacs-flash-state-pattern state)
                    (substring pattern 0 -1)))))

         ;; Проверяем, не метка ли это
         ((emacs-flash-jump-to state char)
          (throw 'done t))

         ;; Добавляем символ к паттерну
         (t
          (setf (emacs-flash-state-pattern state)
                (concat (emacs-flash-state-pattern state)
                        (char-to-string char)))))))))
```

## Порядок реализации

1. **emacs-flash-state.el** — структуры данных
2. **emacs-flash-search.el** — поиск совпадений
3. **emacs-flash-highlight.el** — отображение
4. **emacs-flash-label.el** — назначение меток
5. **emacs-flash-jump.el** — прыжок
6. **emacs-flash.el** — главный модуль и цикл
7. Тестирование базового функционала
8. **emacs-flash-evil.el** — интеграция с evil (после MVP)

## Зависимости

- Emacs >= 27.1 (для `cl-defstruct`, `when-let`)
- `cl-lib` — встроенная библиотека

## Конфигурация по умолчанию

```elisp
(defcustom emacs-flash-labels "asdfjkl;ghqwertyuiopzxcvbnm"
  "Characters to use as jump labels, ordered by priority.")

(defcustom emacs-flash-multi-window t
  "Search in all visible windows.")

(defcustom emacs-flash-autojump t
  "Automatically jump when there is only one match.")

(defcustom emacs-flash-backdrop t
  "Show backdrop effect to dim non-matching text.")

(defcustom emacs-flash-case-fold t
  "Ignore case when searching.")
```
