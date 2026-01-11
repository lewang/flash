;;; emacs-flash-evil.el --- Evil integration for emacs-flash -*- lexical-binding: t -*-

;; Copyright (C) 2025
;; Author: chestnykh
;; Package-Requires: ((emacs "27.1") (evil "1.0"))

;;; Commentary:
;; Evil-mode integration for emacs-flash.
;; Provides motions that work with evil operators (d, y, c, etc.)
;;
;; Usage:
;;   (require 'emacs-flash-evil)
;;   (define-key evil-normal-state-map (kbd "s") #'emacs-flash-evil-jump)
;;
;; Or use the setup function:
;;   (emacs-flash-evil-setup)
;;
;; Remote mode (like flash.nvim):
;;   yr{pattern}{label} - yank at remote location, cursor stays
;;   dr{pattern}{label} - delete at remote location, cursor stays
;;   cr{pattern}{label} - change at remote location, cursor stays

;;; Code:

(require 'evil)
(require 'emacs-flash)
(require 'emacs-flash-char)
(require 'emacs-flash-state)
(require 'emacs-flash-search)
(require 'emacs-flash-label)
(require 'emacs-flash-highlight)
(require 'emacs-flash-jump)

;;; Variables

(defvar emacs-flash-evil-remote--saved-window nil
  "Window before remote operation.")

(defvar emacs-flash-evil-remote--saved-point nil
  "Point before remote operation.")

;;; Evil Motion

(evil-define-motion emacs-flash-evil-jump (count)
  "Flash jump as evil motion.
Works with operators: d s <target> deletes to target.
In visual mode: extends selection to target."
  :type inclusive
  :jump t
  (emacs-flash-jump))

;;; Remote Mode

(defun emacs-flash-evil-remote--save-position ()
  "Save current window and point."
  (setq emacs-flash-evil-remote--saved-window (selected-window))
  (setq emacs-flash-evil-remote--saved-point (point)))

(defun emacs-flash-evil-remote--restore-position ()
  "Restore saved window and point."
  (when (and emacs-flash-evil-remote--saved-window
             (window-live-p emacs-flash-evil-remote--saved-window))
    (select-window emacs-flash-evil-remote--saved-window)
    (when emacs-flash-evil-remote--saved-point
      (goto-char emacs-flash-evil-remote--saved-point)))
  (setq emacs-flash-evil-remote--saved-window nil)
  (setq emacs-flash-evil-remote--saved-point nil))

(evil-define-motion emacs-flash-evil-remote (count)
  "Flash remote motion - jump to target, then restore cursor after operator.
Use with operators: yr{search}{label} yanks at target without moving."
  :type inclusive
  (emacs-flash-evil-remote--save-position)
  (let* ((windows (if emacs-flash-multi-window
                      (window-list nil 'no-minibuf)
                    (list (selected-window))))
         (state (emacs-flash-state-create windows))
         (jumped nil))
    (setf (emacs-flash-state-start-window state) (selected-window))
    (setf (emacs-flash-state-start-point state) (point))
    (unwind-protect
        (setq jumped (emacs-flash-evil-remote--loop state))
      (emacs-flash-highlight-clear state)
      (emacs-flash-state-cleanup state))
    (if jumped
        ;; Schedule restore after operator completes
        (run-at-time 0.01 nil #'emacs-flash-evil-remote--restore-position)
      ;; Cancelled - restore and abort
      (emacs-flash-evil-remote--restore-position)
      (evil-repeat-abort)
      (user-error "Remote operation cancelled"))))

(defun emacs-flash-evil-remote--loop (state)
  "Main input loop for remote STATE.
Returns t if jump was made, nil if cancelled."
  (catch 'emacs-flash-done
    (while t
      ;; Update search results
      (emacs-flash-search state)

      ;; Assign labels
      (if (>= (length (emacs-flash-state-pattern state))
              emacs-flash-min-pattern-length)
          (emacs-flash-label-matches state)
        (dolist (match (emacs-flash-state-matches state))
          (setf (emacs-flash-match-label match) nil)))

      ;; Update display
      (emacs-flash-highlight-update state)

      ;; Autojump if single match
      (when (and emacs-flash-autojump
                 (= (length (emacs-flash-state-matches state)) 1)
                 (>= (length (emacs-flash-state-pattern state))
                     emacs-flash-min-pattern-length))
        (emacs-flash-jump-to-first state)
        (throw 'emacs-flash-done t))

      ;; Read input
      (redisplay t)
      (let* ((pattern (emacs-flash-state-pattern state))
             (prefix (emacs-flash-state-label-prefix state))
             (match-count (length (emacs-flash-state-matches state)))
             (prompt (format "Remote [%s] (%d): %s"
                             (if (string-empty-p pattern) "" pattern)
                             match-count
                             (or prefix "")))
             (char (read-char prompt))
             (char-str (char-to-string char)))
        (cond
         ;; Escape - cancel
         ((= char ?\e)
          (if prefix
              (setf (emacs-flash-state-label-prefix state) nil)
            (throw 'emacs-flash-done nil)))

         ;; Enter - jump to first match
         ((= char ?\r)
          (when (emacs-flash-state-matches state)
            (emacs-flash-jump-to-first state))
          (throw 'emacs-flash-done t))

         ;; Backspace
         ((or (= char ?\C-?) (= char ?\C-h) (= char 127))
          (cond
           (prefix
            (setf (emacs-flash-state-label-prefix state) nil))
           ((> (length pattern) 0)
            (setf (emacs-flash-state-pattern state)
                  (substring pattern 0 -1)))))

         ;; Check if it completes a label
         ((let ((full-label (concat (or prefix "") char-str)))
            (emacs-flash-jump-to-label state full-label))
          (throw 'emacs-flash-done t))

         ;; Check if it's a valid label prefix
         ((emacs-flash-evil-remote--valid-prefix-p state char-str)
          (setf (emacs-flash-state-label-prefix state) char-str))

         ;; Add to pattern
         ((not prefix)
          (setf (emacs-flash-state-pattern state)
                (concat pattern char-str)))

         ;; Invalid input
         (t (beep)))))))

(defun emacs-flash-evil-remote--valid-prefix-p (state prefix)
  "Return t if PREFIX is the start of any multi-char label in STATE."
  (cl-some (lambda (match)
             (let ((label (emacs-flash-match-label match)))
               (and label
                    (> (length label) 1)
                    (string-prefix-p prefix label))))
           (emacs-flash-state-matches state)))

;;; Treesitter-style selection (select semantic units)

(defun emacs-flash-evil-select ()
  "Flash jump and select region from current point to target."
  (interactive)
  (let ((start (point)))
    (when (emacs-flash-jump)
      (evil-visual-make-selection start (point)))))

;;; Setup function

;;;###autoload
(defun emacs-flash-evil-setup (&optional char-motions)
  "Set up evil keybindings for emacs-flash.
Binds 'gs' in normal, visual, motion, and operator states.
Binds 'r' in operator state for remote operations.
When CHAR-MOTIONS is non-nil, also replace f/t/F/T with flash versions."
  (interactive "P")
  ;; Flash jump on gs
  (evil-global-set-key 'normal (kbd "g s") #'emacs-flash-evil-jump)
  (evil-global-set-key 'visual (kbd "g s") #'emacs-flash-evil-jump)
  (evil-global-set-key 'motion (kbd "g s") #'emacs-flash-evil-jump)
  (evil-global-set-key 'operator (kbd "g s") #'emacs-flash-evil-jump)
  ;; Remote mode on 'r' in operator state
  (define-key evil-operator-state-map "r" #'emacs-flash-evil-remote)
  ;; Char motions
  (when char-motions
    (emacs-flash-char-setup-evil-keys))
  (message "emacs-flash-evil: bound 'gs' to flash jump, 'r' to remote%s"
           (if char-motions ", f/t/F/T to flash char" "")))

(provide 'emacs-flash-evil)
;;; emacs-flash-evil.el ends here
