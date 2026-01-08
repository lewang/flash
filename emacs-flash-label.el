;;; emacs-flash-label.el --- Label assignment for emacs-flash -*- lexical-binding: t -*-

;; Copyright (C) 2025
;; Author: chestnykh
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:
;; Smart label assignment with conflict detection.
;; Labels that could continue the search pattern are skipped.

;;; Code:

(require 'cl-lib)
(require 'emacs-flash-state)

;;; Configuration (set by emacs-flash.el)

(defvar emacs-flash-labels)  ; defined in emacs-flash.el

;;; Label Functions

(defun emacs-flash-label-matches (state)
  "Assign labels to matches in STATE.
Labels are assigned to matches sorted by distance from cursor.
Labels that conflict with pattern continuation are skipped."
  (let* ((matches (emacs-flash-state-matches state))
         (pattern (emacs-flash-state-pattern state))
         (labels (emacs-flash--available-labels state pattern))
         (sorted (emacs-flash--sort-by-distance state matches)))
    ;; Reset all labels first
    (dolist (match matches)
      (setf (emacs-flash-match-label match) nil))
    ;; Assign labels to sorted matches
    (cl-loop for match in sorted
             for label in labels
             do (setf (emacs-flash-match-label match) label))))

(defun emacs-flash--available-labels (state pattern)
  "Return labels that won't conflict with PATTERN continuation.
STATE is used to check for conflicts in all search windows."
  (let ((chars (string-to-list emacs-flash-labels)))
    (if (string-empty-p pattern)
        chars
      ;; Skip labels that could continue the pattern
      (cl-remove-if
       (lambda (char)
         (emacs-flash--label-conflicts-p state pattern char))
       chars))))

(defun emacs-flash--label-conflicts-p (state pattern char)
  "Check if CHAR as next input would match text anywhere in buffer.
STATE provides the windows/buffers to search in.
Like flash.nvim, searches entire buffer (not just visible area)."
  (let ((extended (concat pattern (char-to-string char))))
    (cl-some
     (lambda (win)
       (when (window-live-p win)
         (with-current-buffer (window-buffer win)
           (save-excursion
             (goto-char (point-min))
             (let ((case-fold-search t))
               (search-forward extended nil t))))))
     (emacs-flash-state-windows state))))

(defun emacs-flash--sort-by-distance (state matches)
  "Sort MATCHES by distance from cursor position.
STATE provides the reference position via start-point.
Matches at cursor position are sorted last (for continue functionality)."
  (let ((pos (or (emacs-flash-state-start-point state) (point))))
    (sort (copy-sequence matches)
          (lambda (a b)
            (let ((dist-a (abs (- (marker-position (emacs-flash-match-pos a)) pos)))
                  (dist-b (abs (- (marker-position (emacs-flash-match-pos b)) pos))))
              ;; Matches at cursor (distance 0) go last
              (cond
               ((and (= dist-a 0) (= dist-b 0)) nil)
               ((= dist-a 0) nil)  ; a goes after b
               ((= dist-b 0) t)    ; a goes before b
               (t (< dist-a dist-b))))))))

(defun emacs-flash-find-match-by-label (state char)
  "Find match with label CHAR in STATE."
  (cl-find char (emacs-flash-state-matches state)
           :key #'emacs-flash-match-label))

(provide 'emacs-flash-label)
;;; emacs-flash-label.el ends here
