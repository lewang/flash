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
(defvar emacs-flash-multi-char-labels)  ; defined in emacs-flash.el

;;; Label Functions

(defun emacs-flash-label-matches (state)
  "Assign labels to matches in STATE.
Labels are assigned to matches sorted by distance from cursor.
Labels that conflict with pattern continuation are skipped.
Uses multi-char labels when matches exceed available single-char labels."
  (let* ((matches (emacs-flash-state-matches state))
         (pattern (emacs-flash-state-pattern state))
         (available-chars (emacs-flash--available-labels state pattern))
         (sorted (emacs-flash--sort-by-distance state matches))
         (labels (emacs-flash--generate-labels available-chars (length sorted))))
    ;; Reset all labels first
    (dolist (match matches)
      (setf (emacs-flash-match-label match) nil))
    ;; Assign labels to sorted matches
    (cl-loop for match in sorted
             for label in labels
             do (setf (emacs-flash-match-label match) label))))

(defun emacs-flash--generate-labels (chars count)
  "Generate COUNT labels from CHARS.
Returns list of strings. Uses single chars when possible.
When `emacs-flash-multi-char-labels' is non-nil and COUNT > (length CHARS),
generates multi-char labels (aa, as, ad, ...).
When `emacs-flash-multi-char-labels' is nil, excess matches remain unlabeled."
  (let ((n (length chars)))
    (if (<= count n)
        ;; Single char labels
        (mapcar #'char-to-string (cl-subseq chars 0 count))
      ;; More matches than single chars
      (if emacs-flash-multi-char-labels
          ;; Multi-char labels enabled
          (let ((labels nil)
                (needed count))
            ;; Generate two-char combinations
            (catch 'done
              (dolist (c1 chars)
                (dolist (c2 chars)
                  (push (string c1 c2) labels)
                  (cl-decf needed)
                  (when (<= needed 0)
                    (throw 'done nil)))))
            (nreverse labels))
        ;; Multi-char disabled - only use available single chars
        (mapcar #'char-to-string chars)))))

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

(defun emacs-flash-find-match-by-label (state label-str)
  "Find match with exact label LABEL-STR in STATE."
  (cl-find label-str (emacs-flash-state-matches state)
           :key #'emacs-flash-match-label
           :test #'equal))

(defun emacs-flash-matches-with-label-prefix (state prefix)
  "Return matches whose labels start with PREFIX."
  (cl-remove-if-not
   (lambda (match)
     (let ((label (emacs-flash-match-label match)))
       (and label
            (stringp label)
            (string-prefix-p prefix label))))
   (emacs-flash-state-matches state)))

(defun emacs-flash-label-needs-more-chars-p (state)
  "Return t if any match has a multi-char label."
  (cl-some (lambda (match)
             (let ((label (emacs-flash-match-label match)))
               (and label (> (length label) 1))))
           (emacs-flash-state-matches state)))

(provide 'emacs-flash-label)
;;; emacs-flash-label.el ends here
