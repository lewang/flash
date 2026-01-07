;;; emacs-flash-jump.el --- Jump logic for emacs-flash -*- lexical-binding: t -*-

;; Copyright (C) 2025
;; Author: chestnykh
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:
;; Jump to match position, handle window switching and fold unfolding.

;;; Code:

(require 'emacs-flash-state)

;;; Jump Functions

(defun emacs-flash-jump-to-match (match)
  "Jump to MATCH position.
Switches window if needed, unfolds if target is in fold."
  (when match
    (let ((win (emacs-flash-match-window match))
          (pos (emacs-flash-match-pos match))
          (fold (emacs-flash-match-fold match)))
      ;; Switch window if needed
      (unless (eq win (selected-window))
        (select-window win))
      ;; Jump to position
      (goto-char (marker-position pos))
      ;; Unfold if in fold
      (when fold
        (emacs-flash--unfold-at-point))
      t)))

(defun emacs-flash-jump-to-label (state char)
  "Jump to match with label CHAR in STATE.
Returns t if jump successful, nil otherwise."
  (when-let ((match (emacs-flash--find-match-by-label state char)))
    (emacs-flash-jump-to-match match)))

(defun emacs-flash--find-match-by-label (state char)
  "Find match with label CHAR in STATE."
  (cl-find char (emacs-flash-state-matches state)
           :key #'emacs-flash-match-label))

(defun emacs-flash-jump-to-first (state)
  "Jump to first match in STATE.
Returns t if jump successful, nil otherwise."
  (when-let ((match (car (emacs-flash-state-matches state))))
    (emacs-flash-jump-to-match match)))

(defun emacs-flash-return-to-start (state)
  "Return to start position saved in STATE."
  (let ((win (emacs-flash-state-start-window state))
        (pos (emacs-flash-state-start-point state)))
    (when (and win (window-live-p win))
      (select-window win))
    (when pos
      (goto-char pos))))

;;; Fold Handling

(defun emacs-flash--unfold-at-point ()
  "Unfold region at point if folded.
Supports outline-mode, org-mode, and hideshow."
  (cond
   ;; Org-mode
   ((and (derived-mode-p 'org-mode)
         (fboundp 'org-show-context))
    (org-show-context 'link-search))
   ;; Outline-mode
   ((and (or (derived-mode-p 'outline-mode)
             (bound-and-true-p outline-minor-mode))
         (fboundp 'outline-show-entry))
    (outline-show-entry))
   ;; Hideshow
   ((and (bound-and-true-p hs-minor-mode)
         (fboundp 'hs-show-block))
    (hs-show-block))
   ;; Generic: try to make point visible
   (t
    (when (invisible-p (point))
      (let ((inhibit-read-only t))
        (put-text-property (point) (1+ (point)) 'invisible nil))))))

(provide 'emacs-flash-jump)
;;; emacs-flash-jump.el ends here
