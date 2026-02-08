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

(require 'emacs-flash)
(require 'emacs-flash-char)

;;; Forward declarations for optional evil dependency

(declare-function evil-global-set-key "evil")

;;; Evil Motion

;;;###autoload
(defun emacs-flash-evil-jump (&optional _count)
  "Flash jump as evil motion.
Works with operators: d s <target> deletes to target.
In visual mode: extends selection to target."
  (interactive "p")
  (emacs-flash-jump))

(defun emacs-flash-evil--define-motion ()
  "Define `emacs-flash-evil-jump' as an evil motion."
  (eval
   '(evil-define-motion emacs-flash-evil-jump (count)
      "Flash jump as evil motion.
Works with operators: d s <target> deletes to target.
In visual mode: extends selection to target."
      :type inclusive
      :jump t
      (ignore count)
      (emacs-flash-jump))))

(when (require 'evil nil t)
  (emacs-flash-evil--define-motion))

;;; Setup function

;;;###autoload
(defun emacs-flash-evil-setup (&optional char-motions)
  "Set up evil keybindings for emacs-flash.
Binds `gs' in normal, visual, motion, and operator states.
When CHAR-MOTIONS is non-nil, also replace f/t/F/T with flash versions."
  (interactive "P")
  (unless (require 'evil nil t)
    (user-error "Emacs-flash-evil requires the evil package"))
  (emacs-flash-evil--define-motion)
  ;; Flash jump on gs
  (evil-global-set-key 'normal (kbd "g s") #'emacs-flash-evil-jump)
  (evil-global-set-key 'visual (kbd "g s") #'emacs-flash-evil-jump)
  (evil-global-set-key 'motion (kbd "g s") #'emacs-flash-evil-jump)
  (evil-global-set-key 'operator (kbd "g s") #'emacs-flash-evil-jump)
  ;; Char motions
  (when char-motions
    (emacs-flash-char-setup-evil-keys))
  (message "Emacs-flash-evil: bound 'gs' to flash jump%s"
           (if char-motions ", f/t/F/T to flash char" "")))

(provide 'emacs-flash-evil)
;;; emacs-flash-evil.el ends here
