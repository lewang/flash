;;; flash-treesitter.el --- Treesitter integration for flash -*- lexical-binding: t -*-

;; Copyright (C) 2025
;; Author: chestnykh
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:
;; Treesitter-based navigation for flash.
;; Jump to and select treesitter nodes (functions, classes, blocks, etc.)
;;
;; Requires Emacs 29+ with treesitter support.
;;
;; Usage:
;;   M-x flash-treesitter     ; Show labels for nodes at point
;;   M-x flash-treesitter-search  ; Search + show treesitter nodes

;;; Code:

(require 'cl-lib)
(require 'flash-state)
(require 'flash-highlight)

;; Forward declarations
(declare-function treesit-node-at "treesit")
(declare-function treesit-node-parent "treesit")
(declare-function treesit-node-start "treesit")
(declare-function treesit-node-end "treesit")
(declare-function treesit-node-type "treesit")
(declare-function treesit-parser-list "treesit")
(declare-function treesit-buffer-root-node "treesit")
(declare-function treesit-available-p "treesit")

(defvar flash-labels)
(defvar flash-backdrop)

;;; Customization

(defgroup flash-treesitter nil
  "Treesitter navigation for flash."
  :group 'flash
  :prefix "flash-treesitter-")

(defcustom flash-treesitter-max-depth 10
  "Maximum depth of parent nodes to show."
  :type 'integer
  :group 'flash-treesitter)

;;; Node collection

(defun flash-treesitter--available-p ()
  "Return t if treesitter is available in this Emacs."
  (and (fboundp 'treesit-available-p)
       (treesit-available-p)))

(defun flash-treesitter--buffer-has-parser-p ()
  "Return t if current buffer has a treesitter parser."
  (and (flash-treesitter--available-p)
       (treesit-parser-list)))

(defun flash-treesitter--get-nodes-at-point ()
  "Get treesitter node at point and all its parents.
Returns list of (START END TYPE DEPTH) for each node."
  (when (flash-treesitter--buffer-has-parser-p)
    (let ((node (treesit-node-at (point)))
          (nodes '())
          (depth 0))
      (while (and node (< depth flash-treesitter-max-depth))
        (let ((start (treesit-node-start node))
              (end (treesit-node-end node))
              (type (treesit-node-type node)))
          ;; Skip anonymous nodes (punctuation, etc.)
          (when (and type (not (string-prefix-p "_" type)))
            (push (list start end type depth) nodes)))
        (setq node (treesit-node-parent node))
        (setq depth (1+ depth)))
      ;; Return from innermost to outermost (reverse to match flash.nvim)
      (nreverse nodes))))

(defun flash-treesitter--nodes-to-matches (nodes)
  "Convert NODES to flash-match structures with labels."
  (let ((labels flash-labels)
        (matches '())
        (idx 0))
    (dolist (node nodes)
      (let ((start (nth 0 node))
            (end (nth 1 node))
            (label (when (< idx (length labels))
                     (char-to-string (aref labels idx)))))
        (push (make-flash-match
               :pos (copy-marker start)
               :end-pos (copy-marker end)
               :label label
               :window (selected-window)
               :fold nil)
              matches)
        (setq idx (1+ idx))))
    (nreverse matches)))

;;; Highlighting

(defun flash-treesitter--highlight (state)
  "Highlight treesitter nodes in STATE."
  ;; Clear existing overlays
  (flash-highlight-clear state)
  ;; Create backdrop if enabled
  (when flash-backdrop
    (let ((ov (make-overlay (window-start) (window-end))))
      (overlay-put ov 'face 'flash-backdrop)
      (overlay-put ov 'flash t)
      (push ov (flash-state-overlays state))))
  ;; Show labels only (no underline/highlight for nodes - too noisy)
  (dolist (match (flash-state-matches state))
    (let* ((start (marker-position (flash-match-pos match)))
           (label (flash-match-label match)))
      (when label
        (let ((ov (make-overlay start (1+ start))))
          (overlay-put ov 'display (propertize label 'face 'flash-label))
          (overlay-put ov 'flash t)
          (overlay-put ov 'priority 200)
          (push ov (flash-state-overlays state)))))))

;;; Main commands

;;;###autoload
(defun flash-treesitter ()
  "Show labels for treesitter nodes at point.
Press a label to select that node's range."
  (interactive)
  (unless (flash-treesitter--available-p)
    (user-error "Treesitter is not available in this Emacs"))
  (unless (flash-treesitter--buffer-has-parser-p)
    (user-error "No treesitter parser for this buffer"))
  (let* ((nodes (flash-treesitter--get-nodes-at-point))
         (matches (flash-treesitter--nodes-to-matches nodes))
         (state (flash-state-create (list (selected-window)))))
    (unless nodes
      (user-error "No treesitter nodes at point"))
    (setf (flash-state-matches state) matches)
    (setf (flash-state-start-window state) (selected-window))
    (setf (flash-state-start-point state) (point))
    (unwind-protect
        (flash-treesitter--loop state)
      (flash-state-cleanup state))))

(defun flash-treesitter--loop (state)
  "Input loop for treesitter STATE."
  (catch 'flash-ts-done
    (while t
      (flash-treesitter--highlight state)
      (redisplay t)
      (let ((char (read-char "Treesitter node: ")))
        (cond
         ;; Escape - cancel
         ((= char ?\e)
          (throw 'flash-ts-done nil))
         ;; Enter - select innermost (first) node
         ((= char ?\r)
          (when-let ((match (car (flash-state-matches state))))
            (flash-treesitter--select-match match))
          (throw 'flash-ts-done t))
         ;; Try to find matching label
         (t
          (let* ((char-str (char-to-string char))
                 (match (cl-find-if (lambda (m)
                                      (equal (flash-match-label m) char-str))
                                    (flash-state-matches state))))
            (if match
                (progn
                  (flash-treesitter--select-match match)
                  (throw 'flash-ts-done t))
              (beep)))))))))

(defun flash-treesitter--select-match (match)
  "Select the range of MATCH."
  (let ((start (marker-position (flash-match-pos match)))
        (end (marker-position (flash-match-end-pos match))))
    ;; Set mark and move point to create selection
    (goto-char start)
    (push-mark end t t)
    (message "Selected %d characters" (- end start))))

;;; Evil integration

(declare-function evil-visual-make-region "evil-visual")
(declare-function evil-visual-char "evil-commands")
(defvar evil-visual-char)

;;;###autoload
(defun flash-treesitter-evil ()
  "Treesitter node selection with evil visual mode."
  (interactive)
  (unless (flash-treesitter--available-p)
    (user-error "Treesitter is not available in this Emacs"))
  (unless (flash-treesitter--buffer-has-parser-p)
    (user-error "No treesitter parser for this buffer"))
  (let* ((nodes (flash-treesitter--get-nodes-at-point))
         (matches (flash-treesitter--nodes-to-matches nodes))
         (state (flash-state-create (list (selected-window)))))
    (unless nodes
      (user-error "No treesitter nodes at point"))
    (setf (flash-state-matches state) matches)
    (setf (flash-state-start-window state) (selected-window))
    (setf (flash-state-start-point state) (point))
    (unwind-protect
        (flash-treesitter--evil-loop state)
      (flash-state-cleanup state))))

(defun flash-treesitter--evil-loop (state)
  "Input loop for treesitter STATE with evil support."
  (catch 'flash-ts-done
    (while t
      (flash-treesitter--highlight state)
      (redisplay t)
      (let ((char (read-char "Treesitter node: ")))
        (cond
         ;; Escape - cancel
         ((= char ?\e)
          (throw 'flash-ts-done nil))
         ;; Enter - select innermost node
         ((= char ?\r)
          (when-let ((match (car (flash-state-matches state))))
            (flash-treesitter--evil-select match))
          (throw 'flash-ts-done t))
         ;; Try to find matching label
         (t
          (let* ((char-str (char-to-string char))
                 (match (cl-find-if (lambda (m)
                                      (equal (flash-match-label m) char-str))
                                    (flash-state-matches state))))
            (if match
                (progn
                  (flash-treesitter--evil-select match)
                  (throw 'flash-ts-done t))
              (beep)))))))))

(defun flash-treesitter--evil-select (match)
  "Select the range of MATCH using evil visual mode."
  (let ((start (marker-position (flash-match-pos match)))
        (end (marker-position (flash-match-end-pos match))))
    (if (featurep 'evil)
        (progn
          (goto-char start)
          (evil-visual-char)
          (goto-char (1- end)))
      ;; Fallback for non-evil
      (goto-char start)
      (push-mark end t t))))

(provide 'flash-treesitter)
;;; flash-treesitter.el ends here
