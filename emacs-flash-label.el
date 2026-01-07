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
  "Check if CHAR as next input would match text after PATTERN.
STATE provides the windows to search in."
  (let ((extended (concat pattern (char-to-string char))))
    (cl-some
     (lambda (win)
       (when (window-live-p win)
         (with-selected-window win
           (save-excursion
             (goto-char (window-start win))
             (let ((case-fold-search t))
               (search-forward extended (window-end win t) t))))))
     (emacs-flash-state-windows state))))

(defun emacs-flash--sort-by-distance (state matches)
  "Sort MATCHES by distance from cursor position.
STATE provides the reference position via start-point."
  (let ((pos (or (emacs-flash-state-start-point state) (point))))
    (sort (copy-sequence matches)
          (lambda (a b)
            (< (abs (- (marker-position (emacs-flash-match-pos a)) pos))
               (abs (- (marker-position (emacs-flash-match-pos b)) pos)))))))

(defun emacs-flash-find-match-by-label (state char)
  "Find match with label CHAR in STATE."
  (cl-find char (emacs-flash-state-matches state)
           :key #'emacs-flash-match-label))

(provide 'emacs-flash-label)
;;; emacs-flash-label.el ends here
