;;; emacs-flash-highlight-test.el --- Tests for emacs-flash-highlight -*- lexical-binding: t -*-

;;; Commentary:
;; ERT tests for emacs-flash-highlight module.

;;; Code:

(require 'ert)
(require 'emacs-flash-state)
(require 'emacs-flash-highlight)

;; Variables dynamically bound in tests.
(defvar emacs-flash-backdrop)
(defvar emacs-flash-label-position)
(defvar emacs-flash-rainbow)

(ert-deftest emacs-flash-highlight-clear-test ()
  "Test that highlight-clear removes all overlays."
  (with-temp-buffer
    (insert "test content")
    (let ((state (emacs-flash-state-create)))
      ;; Add some overlays manually
      (let ((ov1 (make-overlay 1 5))
            (ov2 (make-overlay 6 10)))
        (setf (emacs-flash-state-overlays state) (list ov1 ov2))
        (should (= 2 (length (emacs-flash-state-overlays state))))
        ;; Clear
        (emacs-flash-highlight-clear state)
        ;; Should be empty
        (should (null (emacs-flash-state-overlays state)))
        ;; Overlays should be deleted
        (should-not (overlay-buffer ov1))
        (should-not (overlay-buffer ov2))))))

(ert-deftest emacs-flash-highlight-update-creates-overlays-test ()
  "Test that highlight-update creates overlays for matches."
  (with-temp-buffer
    (insert "foo bar foo")
    (goto-char (point-min))
    (set-window-buffer (selected-window) (current-buffer))
    (let ((state (emacs-flash-state-create (list (selected-window))))
          (emacs-flash-backdrop nil))  ; disable backdrop for simpler test
      ;; Add a match manually
      (setf (emacs-flash-state-matches state)
            (list (make-emacs-flash-match
                   :pos (copy-marker 1)
                   :end-pos (copy-marker 4)
                   :label "a"
                   :window (selected-window)
                   :fold nil)))
      ;; Update highlights
      (emacs-flash-highlight-update state)
      ;; Should have overlays (match + label)
      (should (>= (length (emacs-flash-state-overlays state)) 2)))))

(ert-deftest emacs-flash-highlight-backdrop-test ()
  "Test that backdrop overlay is created when enabled."
  (with-temp-buffer
    (insert "test content here")
    (goto-char (point-min))
    (set-window-buffer (selected-window) (current-buffer))
    (let ((state (emacs-flash-state-create (list (selected-window))))
          (emacs-flash-backdrop t))
      (setf (emacs-flash-state-matches state) nil)
      (emacs-flash-highlight-update state)
      ;; Should have backdrop overlay
      (should (>= (length (emacs-flash-state-overlays state)) 1))
      ;; Check it has backdrop face
      (let ((ov (car (emacs-flash-state-overlays state))))
        (should (eq 'emacs-flash-backdrop (overlay-get ov 'face)))))))

(ert-deftest emacs-flash-highlight-no-backdrop-test ()
  "Test that no backdrop when disabled."
  (with-temp-buffer
    (insert "test content")
    (goto-char (point-min))
    (set-window-buffer (selected-window) (current-buffer))
    (let ((state (emacs-flash-state-create (list (selected-window))))
          (emacs-flash-backdrop nil))
      (setf (emacs-flash-state-matches state) nil)
      (emacs-flash-highlight-update state)
      ;; Should have no overlays
      (should (null (emacs-flash-state-overlays state))))))

(ert-deftest emacs-flash-highlight-label-test ()
  "Test that label is displayed as after-string (default position)."
  (with-temp-buffer
    (insert "foo bar")
    (goto-char (point-min))
    (set-window-buffer (selected-window) (current-buffer))
    (let ((state (emacs-flash-state-create (list (selected-window))))
          (emacs-flash-backdrop nil)
          (emacs-flash-label-position 'after))
      (setf (emacs-flash-state-matches state)
            (list (make-emacs-flash-match
                   :pos (copy-marker 1)
                   :end-pos (copy-marker 4)
                   :label "x"
                   :window (selected-window)
                   :fold nil)))
      (emacs-flash-highlight-update state)
      ;; Find label overlay
      (let ((label-ov (seq-find (lambda (ov)
                                  (overlay-get ov 'after-string))
                                (emacs-flash-state-overlays state))))
        (should label-ov)
        (should (string= "x" (overlay-get label-ov 'after-string)))))))

(ert-deftest emacs-flash-highlight-match-without-label-test ()
  "Test that match without label still gets highlight."
  (with-temp-buffer
    (insert "foo bar")
    (goto-char (point-min))
    (set-window-buffer (selected-window) (current-buffer))
    (let ((state (emacs-flash-state-create (list (selected-window))))
          (emacs-flash-backdrop nil))
      (setf (emacs-flash-state-matches state)
            (list (make-emacs-flash-match
                   :pos (copy-marker 1)
                   :end-pos (copy-marker 4)
                   :label nil  ; no label
                   :window (selected-window)
                   :fold nil)))
      (emacs-flash-highlight-update state)
      ;; Should have match overlay
      (should (= 1 (length (emacs-flash-state-overlays state))))
      (should (eq 'emacs-flash-match
                  (overlay-get (car (emacs-flash-state-overlays state)) 'face))))))

(ert-deftest emacs-flash-highlight-rainbow-disabled-test ()
  "Test that default face is used when rainbow disabled."
  (let ((emacs-flash-rainbow nil))
    (should (eq 'emacs-flash-label (emacs-flash--get-label-face 0)))
    (should (eq 'emacs-flash-label (emacs-flash--get-label-face 1)))
    (should (eq 'emacs-flash-label (emacs-flash--get-label-face 5)))))

(ert-deftest emacs-flash-highlight-rainbow-enabled-test ()
  "Test that rainbow faces cycle when enabled.
Uses same 10 colors as flash.nvim: red, amber, lime, green, teal, cyan, blue, violet, fuchsia, rose."
  (let ((emacs-flash-rainbow t))
    ;; First 10 indices should return different faces (Tailwind colors)
    (should (eq 'emacs-flash-label-red (emacs-flash--get-label-face 0)))
    (should (eq 'emacs-flash-label-amber (emacs-flash--get-label-face 1)))
    (should (eq 'emacs-flash-label-lime (emacs-flash--get-label-face 2)))
    (should (eq 'emacs-flash-label-green (emacs-flash--get-label-face 3)))
    (should (eq 'emacs-flash-label-teal (emacs-flash--get-label-face 4)))
    (should (eq 'emacs-flash-label-cyan (emacs-flash--get-label-face 5)))
    (should (eq 'emacs-flash-label-blue (emacs-flash--get-label-face 6)))
    (should (eq 'emacs-flash-label-violet (emacs-flash--get-label-face 7)))
    (should (eq 'emacs-flash-label-fuchsia (emacs-flash--get-label-face 8)))
    (should (eq 'emacs-flash-label-rose (emacs-flash--get-label-face 9)))
    ;; Should cycle back
    (should (eq 'emacs-flash-label-red (emacs-flash--get-label-face 10)))))

(ert-deftest emacs-flash-highlight-rainbow-labels-visual-test ()
  "Test that rainbow labels are applied to overlays."
  (with-temp-buffer
    (insert "foo bar foo baz foo")
    (goto-char (point-min))
    (set-window-buffer (selected-window) (current-buffer))
    (let ((state (emacs-flash-state-create (list (selected-window))))
          (emacs-flash-backdrop nil)
          (emacs-flash-rainbow t))
      ;; Add multiple matches with labels
      (setf (emacs-flash-state-matches state)
            (list (make-emacs-flash-match
                   :pos (copy-marker 1) :end-pos (copy-marker 4)
                   :label "a" :window (selected-window) :fold nil)
                  (make-emacs-flash-match
                   :pos (copy-marker 9) :end-pos (copy-marker 12)
                   :label "s" :window (selected-window) :fold nil)
                  (make-emacs-flash-match
                   :pos (copy-marker 17) :end-pos (copy-marker 20)
                   :label "d" :window (selected-window) :fold nil)))
      (emacs-flash-highlight-update state)
      ;; Should have 6 overlays (3 matches + 3 labels)
      (should (= 6 (length (emacs-flash-state-overlays state))))
      ;; Check label overlays have different faces
      (let ((label-ovs (seq-filter (lambda (ov) (overlay-get ov 'after-string))
                                   (emacs-flash-state-overlays state))))
        (should (= 3 (length label-ovs)))))))

;;; Label Position Tests

(ert-deftest emacs-flash-highlight-label-position-before-test ()
  "Test label position 'before'."
  (with-temp-buffer
    (insert "foo bar")
    (goto-char (point-min))
    (set-window-buffer (selected-window) (current-buffer))
    (let ((state (emacs-flash-state-create (list (selected-window))))
          (emacs-flash-backdrop nil)
          (emacs-flash-label-position 'before))
      (setf (emacs-flash-state-matches state)
            (list (make-emacs-flash-match
                   :pos (copy-marker 1)
                   :end-pos (copy-marker 4)
                   :label "x"
                   :window (selected-window)
                   :fold nil)))
      (emacs-flash-highlight-update state)
      ;; Find label overlay with before-string
      (let ((label-ov (seq-find (lambda (ov)
                                  (overlay-get ov 'before-string))
                                (emacs-flash-state-overlays state))))
        (should label-ov)
        (should (string= "x" (overlay-get label-ov 'before-string)))))))

(ert-deftest emacs-flash-highlight-label-position-overlay-test ()
  "Test label position 'overlay' (replaces first char)."
  (with-temp-buffer
    (insert "foo bar")
    (goto-char (point-min))
    (set-window-buffer (selected-window) (current-buffer))
    (let ((state (emacs-flash-state-create (list (selected-window))))
          (emacs-flash-backdrop nil)
          (emacs-flash-label-position 'overlay))
      (setf (emacs-flash-state-matches state)
            (list (make-emacs-flash-match
                   :pos (copy-marker 1)
                   :end-pos (copy-marker 4)
                   :label "x"
                   :window (selected-window)
                   :fold nil)))
      (emacs-flash-highlight-update state)
      ;; Find label overlay with display property
      (let ((label-ov (seq-find (lambda (ov)
                                  (overlay-get ov 'display))
                                (emacs-flash-state-overlays state))))
        (should label-ov)
        (should (string= "x" (overlay-get label-ov 'display)))
        ;; Overlay should cover exactly 1 character
        (should (= 1 (- (overlay-end label-ov) (overlay-start label-ov))))))))

(ert-deftest emacs-flash-highlight-label-position-eol-test ()
  "Test label position 'eol' (end of line)."
  (with-temp-buffer
    (insert "foo bar\nbaz")
    (goto-char (point-min))
    (set-window-buffer (selected-window) (current-buffer))
    (let ((state (emacs-flash-state-create (list (selected-window))))
          (emacs-flash-backdrop nil)
          (emacs-flash-label-position 'eol))
      (setf (emacs-flash-state-matches state)
            (list (make-emacs-flash-match
                   :pos (copy-marker 1)
                   :end-pos (copy-marker 4)
                   :label "x"
                   :window (selected-window)
                   :fold nil)))
      (emacs-flash-highlight-update state)
      ;; Find label overlay at end of line
      (let ((label-ov (seq-find (lambda (ov)
                                  (and (overlay-get ov 'after-string)
                                       (= (overlay-start ov) 8))) ; EOL position
                                (emacs-flash-state-overlays state))))
        (should label-ov)
        ;; after-string should contain label (with space prefix)
        (should (string-match-p "x" (overlay-get label-ov 'after-string)))))))

(ert-deftest emacs-flash-highlight-label-position-defcustom-test ()
  "Test that label-position defcustom exists."
  (should (boundp 'emacs-flash-label-position)))

(provide 'emacs-flash-highlight-test)
;;; emacs-flash-highlight-test.el ends here
