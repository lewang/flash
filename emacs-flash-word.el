;;; emacs-flash-word.el --- Word mode for emacs-flash -*- lexical-binding: t -*-

;; Copyright (C) 2025
;; Author: chestnykh
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:
;; Jump to the beginning of any visible word without typing a search pattern.
;; Similar to avy-goto-word but uses flash infrastructure.

;;; Code:

(require 'cl-lib)
(require 'emacs-flash-state)
(require 'emacs-flash-label)
(require 'emacs-flash-highlight)
(require 'emacs-flash-jump)

;;; Configuration (from emacs-flash.el)

(defvar emacs-flash-multi-window)
(defvar emacs-flash-labels)

;;; Word Mode Functions

;;;###autoload
(defun emacs-flash-word-jump ()
  "Jump to the beginning of any visible word.
Shows labels at the start of each word in visible windows.
Press a label key to jump to that word."
  (interactive)
  (let ((windows (if (bound-and-true-p emacs-flash-multi-window)
                     (window-list nil 'no-minibuf)
                   (list (selected-window)))))
    (let ((state (emacs-flash-state-create windows)))
      (setf (emacs-flash-state-start-window state) (selected-window))
      (setf (emacs-flash-state-start-point state) (point))
      (unwind-protect
          (emacs-flash-word--run state)
        (emacs-flash-highlight-clear state)
        (emacs-flash-state-cleanup state)))))

(defun emacs-flash-word--run (state)
  "Run word mode with STATE."
  ;; Collect all visible word starts as matches
  (emacs-flash-word--collect-words state)
  ;; Assign labels
  (emacs-flash-label-matches state)
  ;; Show highlights
  (emacs-flash-highlight-update state)
  ;; Read input and jump
  (emacs-flash-word--input-loop state))

(defun emacs-flash-word--collect-words (state)
  "Collect word start positions in all windows of STATE."
  (let ((matches nil))
    (dolist (win (emacs-flash-state-windows state))
      (when (window-live-p win)
        (with-current-buffer (window-buffer win)
          (let ((start (window-start win))
                (end (window-end win t)))
            (save-excursion
              (goto-char start)
              ;; Find all word starts
              (while (and (< (point) end)
                          (re-search-forward "\\<\\w" end t))
                (let ((word-start (match-beginning 0)))
                  ;; Only add if not invisible
                  (unless (invisible-p word-start)
                    (push (make-emacs-flash-match
                           :pos (copy-marker word-start)
                           :end-pos (copy-marker (1+ word-start))
                           :label nil
                           :window win
                           :fold nil)
                          matches)))))))))
    ;; Store matches sorted by position
    (setf (emacs-flash-state-matches state) (nreverse matches))))

(defun emacs-flash-word--input-loop (state)
  "Input loop for word mode STATE.
Returns t if jump was made, nil if cancelled."
  (catch 'emacs-flash-word-done
    (while t
      (redisplay t)
      (let* ((prefix (emacs-flash-state-label-prefix state))
             (prompt (if prefix
                         (format "Word [%s]: " prefix)
                       "Word: "))
             (char (read-char prompt))
             (char-str (char-to-string char)))
        (cond
         ;; Escape - cancel (or clear prefix)
         ((= char ?\e)
          (if prefix
              (progn
                (setf (emacs-flash-state-label-prefix state) nil)
                ;; Re-show all labels
                (emacs-flash-highlight-update state))
            (emacs-flash-return-to-start state)
            (throw 'emacs-flash-word-done nil)))

         ;; Check if it completes a label
         ((let ((full-label (concat (or prefix "") char-str)))
            (emacs-flash-jump-to-label state full-label))
          (throw 'emacs-flash-word-done t))

         ;; Check if it's a valid label prefix
         ((emacs-flash-word--valid-prefix-p state char-str)
          (setf (emacs-flash-state-label-prefix state) char-str)
          ;; Update display to show filtered labels
          (emacs-flash-highlight-update state))

         ;; Invalid input
         (t (beep)))))))

(defun emacs-flash-word--valid-prefix-p (state prefix)
  "Return t if PREFIX is the start of any multi-char label in STATE."
  (cl-some (lambda (match)
             (let ((label (emacs-flash-match-label match)))
               (and label
                    (> (length label) 1)
                    (string-prefix-p prefix label))))
           (emacs-flash-state-matches state)))

(provide 'emacs-flash-word)
;;; emacs-flash-word.el ends here
