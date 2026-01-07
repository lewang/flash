;;; emacs-flash-search.el --- Search functionality for emacs-flash -*- lexical-binding: t -*-

;; Copyright (C) 2025
;; Author: chestnykh
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:
;; Search for pattern matches in visible windows.

;;; Code:

(require 'emacs-flash-state)

(defun emacs-flash-search (state)
  "Find all matches for STATE pattern in all windows.
Updates STATE matches field with found matches."
  (let ((pattern (emacs-flash-state-pattern state))
        (windows (emacs-flash-state-windows state))
        (case-fold-search t)  ; case-insensitive by default
        matches)
    (when (> (length pattern) 0)
      (dolist (win windows)
        (with-selected-window win
          (save-excursion
            (goto-char (window-start win))
            (let ((limit (window-end win t)))
              (while (search-forward pattern limit t)
                (let* ((pos (match-beginning 0))
                       (end-pos (match-end 0))
                       (fold (emacs-flash--get-fold-at pos)))
                  (push (make-emacs-flash-match
                         :pos (copy-marker pos)
                         :end-pos (copy-marker end-pos)
                         :label nil
                         :window win
                         :fold fold)
                        matches))))))))
    (setf (emacs-flash-state-matches state) (nreverse matches))))

(defun emacs-flash--get-fold-at (pos)
  "Return fold start position if POS is in invisible region, nil otherwise."
  (when (invisible-p pos)
    (previous-single-property-change (1+ pos) 'invisible nil (point-min))))

(provide 'emacs-flash-search)
;;; emacs-flash-search.el ends here
