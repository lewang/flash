;;; emacs-flash.el --- Flash-style navigation for Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2025
;; Author: chestnykh
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience, navigation
;; URL: https://github.com/chestnykh/emacs-flash

;;; Commentary:
;; Fast navigation using labels assigned to search matches.
;; Similar to flash.nvim for Neovim.
;;
;; Features:
;; - Incremental search with instant feedback
;; - Smart labels that skip conflicting characters
;; - Multi-window search support
;; - Autojump when single match
;; - Fold-aware navigation
;;
;; Usage:
;;   M-x emacs-flash-jump
;;
;; Or bind to a key:
;;   (global-set-key (kbd "s-j") #'emacs-flash-jump)

;;; Code:

(require 'cl-lib)
(require 'emacs-flash-state)
(require 'emacs-flash-search)
(require 'emacs-flash-label)
(require 'emacs-flash-highlight)
(require 'emacs-flash-jump)

;;; Customization

(defgroup emacs-flash nil
  "Flash-style navigation."
  :group 'convenience
  :prefix "emacs-flash-")

(defcustom emacs-flash-labels "asdfjkl;ghqwertyuiopzxcvbnm"
  "Characters to use as jump labels, ordered by priority.
Earlier characters are assigned to closer matches."
  :type 'string
  :group 'emacs-flash)

(defcustom emacs-flash-multi-window t
  "When non-nil, search in all visible windows."
  :type 'boolean
  :group 'emacs-flash)

(defcustom emacs-flash-autojump t
  "When non-nil, automatically jump when there is only one match."
  :type 'boolean
  :group 'emacs-flash)

(defcustom emacs-flash-backdrop t
  "When non-nil, dim non-matching text with backdrop effect."
  :type 'boolean
  :group 'emacs-flash)

(defcustom emacs-flash-case-fold t
  "When non-nil, ignore case when searching."
  :type 'boolean
  :group 'emacs-flash)

(defcustom emacs-flash-rainbow nil
  "When non-nil, use different colors for each label.
Makes labels more visually distinct."
  :type 'boolean
  :group 'emacs-flash)

(defcustom emacs-flash-highlight-matches t
  "When non-nil, highlight matched text.
When nil, only labels are shown, keeping original syntax highlighting."
  :type 'boolean
  :group 'emacs-flash)

(defcustom emacs-flash-label-position 'after
  "Where to display the jump label relative to the match.
- `after': Label appears after the match (default, like flash.nvim)
- `before': Label appears before the match
- `overlay': Label replaces the first character of the match
- `eol': Label appears at end of line"
  :type '(choice (const :tag "After match" after)
                 (const :tag "Before match" before)
                 (const :tag "Over first char" overlay)
                 (const :tag "End of line" eol))
  :group 'emacs-flash)

;;; Main Command

;;;###autoload
(defun emacs-flash-jump ()
  "Start flash jump session.
Type characters to search, then press a label to jump.
Press RET to jump to first match, ESC to cancel."
  (interactive)
  (let ((windows (if emacs-flash-multi-window
                     (window-list nil 'no-minibuf)
                   (list (selected-window)))))
    (let ((state (emacs-flash-state-create windows)))
      (setf (emacs-flash-state-start-window state) (selected-window))
      (setf (emacs-flash-state-start-point state) (point))
      (unwind-protect
          (emacs-flash--loop state)
        (emacs-flash-highlight-clear state)
        (emacs-flash-state-cleanup state)))))

;;; Main Loop

(defun emacs-flash--loop (state)
  "Main input loop for STATE.
Returns t if jump was made, nil if cancelled."
  (catch 'emacs-flash-done
    (while t
      ;; Update search results
      (emacs-flash-search state)

      ;; Assign labels
      (emacs-flash-label-matches state)

      ;; Update display
      (emacs-flash-highlight-update state)

      ;; Autojump if single match
      (when (and emacs-flash-autojump
                 (= (length (emacs-flash-state-matches state)) 1))
        (emacs-flash-jump-to-first state)
        (throw 'emacs-flash-done t))

      ;; Read input
      (let* ((pattern (emacs-flash-state-pattern state))
             (match-count (length (emacs-flash-state-matches state)))
             (prompt (emacs-flash--format-prompt pattern match-count))
             (char (read-char prompt)))
        (cond
         ;; Escape - cancel
         ((= char ?\e)
          (emacs-flash-return-to-start state)
          (throw 'emacs-flash-done nil))

         ;; Enter - jump to first match
         ((= char ?\r)
          (when (emacs-flash-state-matches state)
            (emacs-flash-jump-to-first state))
          (throw 'emacs-flash-done t))

         ;; Backspace - delete last char
         ((or (= char ?\C-?) (= char ?\C-h) (= char 127))
          (when (> (length pattern) 0)
            (setf (emacs-flash-state-pattern state)
                  (substring pattern 0 -1))))

         ;; Check if it's a label
         ((emacs-flash-jump-to-label state char)
          (throw 'emacs-flash-done t))

         ;; Add to pattern
         (t
          (setf (emacs-flash-state-pattern state)
                (concat pattern (char-to-string char)))))))))

(defun emacs-flash--format-prompt (pattern match-count)
  "Format prompt string showing PATTERN and MATCH-COUNT."
  (if (string-empty-p pattern)
      "Flash: "
    (format "Flash [%s] (%d): " pattern match-count)))

(provide 'emacs-flash)
;;; emacs-flash.el ends here
