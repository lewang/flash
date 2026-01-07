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

;;; Code:

(require 'evil)
(require 'emacs-flash)

;;; Evil Motion

(evil-define-motion emacs-flash-evil-jump (count)
  "Flash jump as evil motion.
Works with operators: d s <target> deletes to target.
In visual mode: extends selection to target."
  :type inclusive
  :jump t
  (emacs-flash-jump))

;;; Remote Operations (like flash.nvim remote)

(evil-define-operator emacs-flash-evil-yank-remote (beg end type)
  "Yank text at flash target without moving cursor."
  :motion nil
  :move-point nil
  (interactive "<R>")
  (let ((start-point (point))
        (start-window (selected-window)))
    (when (emacs-flash-jump)
      ;; Select word at target and yank
      (evil-visual-char)
      (evil-inner-word)
      (evil-yank (region-beginning) (region-end))
      (evil-exit-visual-state)
      ;; Return to start
      (select-window start-window)
      (goto-char start-point)
      (message "Yanked from remote location"))))

(evil-define-operator emacs-flash-evil-paste-remote (beg end type)
  "Paste at flash target without moving cursor."
  :motion nil
  :move-point nil
  (interactive "<R>")
  (let ((start-point (point))
        (start-window (selected-window)))
    (when (emacs-flash-jump)
      ;; Paste at target
      (evil-paste-after 1)
      ;; Return to start
      (select-window start-window)
      (goto-char start-point)
      (message "Pasted at remote location"))))

;;; Treesitter-style selection (select semantic units)

(defun emacs-flash-evil-select ()
  "Flash jump and select region from current point to target."
  (interactive)
  (let ((start (point)))
    (when (emacs-flash-jump)
      (evil-visual-make-selection start (point)))))

;;; Setup function

;;;###autoload
(defun emacs-flash-evil-setup ()
  "Set up evil keybindings for emacs-flash.
Binds 'gs' in normal, visual, motion, and operator states."
  (interactive)
  (evil-global-set-key 'normal (kbd "g s") #'emacs-flash-evil-jump)
  (evil-global-set-key 'visual (kbd "g s") #'emacs-flash-evil-jump)
  (evil-global-set-key 'motion (kbd "g s") #'emacs-flash-evil-jump)
  (evil-global-set-key 'operator (kbd "g s") #'emacs-flash-evil-jump)
  (message "emacs-flash-evil: bound 'gs' to flash jump"))

(provide 'emacs-flash-evil)
;;; emacs-flash-evil.el ends here
