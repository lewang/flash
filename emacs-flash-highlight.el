;;; emacs-flash-highlight.el --- Highlighting for emacs-flash -*- lexical-binding: t -*-

;; Copyright (C) 2025
;; Author: chestnykh
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:
;; Visual feedback using overlays: backdrop, match highlighting, labels.

;;; Code:

(require 'emacs-flash-state)

;;; Faces

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

;;; Configuration (set by emacs-flash.el)

(defvar emacs-flash-backdrop)  ; defined in emacs-flash.el

;;; Highlight Functions

(defun emacs-flash-highlight-update (state)
  "Update highlighting for STATE.
Clears old overlays and creates new ones for backdrop, matches, and labels."
  ;; Remove old overlays
  (emacs-flash-highlight-clear state)

  ;; Backdrop
  (when emacs-flash-backdrop
    (emacs-flash--highlight-backdrop state))

  ;; Matches and labels
  (dolist (match (emacs-flash-state-matches state))
    (emacs-flash--highlight-match state match)))

(defun emacs-flash-highlight-clear (state)
  "Remove all overlays from STATE."
  (mapc #'delete-overlay (emacs-flash-state-overlays state))
  (setf (emacs-flash-state-overlays state) nil))

(defun emacs-flash--highlight-backdrop (state)
  "Add backdrop overlay to all windows in STATE."
  (dolist (win (emacs-flash-state-windows state))
    (when (window-live-p win)
      (with-selected-window win
        (let ((ov (make-overlay (window-start win) (window-end win t))))
          (overlay-put ov 'face 'emacs-flash-backdrop)
          (overlay-put ov 'emacs-flash t)
          (overlay-put ov 'priority 0)
          (push ov (emacs-flash-state-overlays state)))))))

(defun emacs-flash--highlight-match (state match)
  "Add overlays for MATCH to STATE."
  (let ((pos (emacs-flash-match-pos match))
        (end-pos (emacs-flash-match-end-pos match))
        (label (emacs-flash-match-label match))
        (fold (emacs-flash-match-fold match)))
    ;; For folded matches, show label at fold line
    (let ((display-pos (if fold fold pos))
          (display-end (if fold (1+ fold) end-pos)))
      ;; Match highlight
      (let ((ov (make-overlay display-pos display-end)))
        (overlay-put ov 'face 'emacs-flash-match)
        (overlay-put ov 'emacs-flash t)
        (overlay-put ov 'priority 100)
        (push ov (emacs-flash-state-overlays state)))
      ;; Label (shown after match)
      (when label
        (let ((ov (make-overlay display-end display-end)))
          (overlay-put ov 'after-string
                       (propertize (char-to-string label)
                                   'face 'emacs-flash-label))
          (overlay-put ov 'emacs-flash t)
          (overlay-put ov 'priority 200)
          (push ov (emacs-flash-state-overlays state)))))))

(provide 'emacs-flash-highlight)
;;; emacs-flash-highlight.el ends here
