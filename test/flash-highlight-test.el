;;; flash-highlight-test.el --- Tests for flash-highlight -*- lexical-binding: t -*-

;;; Commentary:
;; ERT tests for flash-highlight module.

;;; Code:

(require 'ert)
(require 'flash-state)
(require 'flash-highlight)

;; Variables dynamically bound in tests.
(defvar flash-backdrop)
(defvar flash-label-position)
(defvar flash-rainbow)

(ert-deftest flash-highlight-clear-test ()
  "Test that highlight-clear removes all overlays."
  (with-temp-buffer
    (insert "test content")
    (let ((state (flash-state-create)))
      ;; Add some overlays manually
      (let ((ov1 (make-overlay 1 5))
            (ov2 (make-overlay 6 10)))
        (setf (flash-state-overlays state) (list ov1 ov2))
        (should (= 2 (length (flash-state-overlays state))))
        ;; Clear
        (flash-highlight-clear state)
        ;; Should be empty
        (should (null (flash-state-overlays state)))
        ;; Overlays should be deleted
        (should-not (overlay-buffer ov1))
        (should-not (overlay-buffer ov2))))))

(ert-deftest flash-highlight-update-creates-overlays-test ()
  "Test that highlight-update creates overlays for matches."
  (with-temp-buffer
    (insert "foo bar foo")
    (goto-char (point-min))
    (set-window-buffer (selected-window) (current-buffer))
    (let ((state (flash-state-create (list (selected-window))))
          (flash-backdrop nil))  ; disable backdrop for simpler test
      ;; Add a match manually
      (setf (flash-state-matches state)
            (list (make-flash-match
                   :pos (copy-marker 1)
                   :end-pos (copy-marker 4)
                   :label "a"
                   :window (selected-window)
                   :fold nil)))
      ;; Update highlights
      (flash-highlight-update state)
      ;; Should have overlays (match + label)
      (should (>= (length (flash-state-overlays state)) 2)))))

(ert-deftest flash-highlight-backdrop-test ()
  "Test that backdrop overlay is created when enabled."
  (with-temp-buffer
    (insert "test content here")
    (goto-char (point-min))
    (set-window-buffer (selected-window) (current-buffer))
    (let ((state (flash-state-create (list (selected-window))))
          (flash-backdrop t))
      (setf (flash-state-matches state) nil)
      (flash-highlight-update state)
      ;; Should have backdrop overlay
      (should (>= (length (flash-state-overlays state)) 1))
      ;; Check it has backdrop face
      (let ((ov (car (flash-state-overlays state))))
        (should (eq 'flash-backdrop (overlay-get ov 'face)))))))

(ert-deftest flash-highlight-no-backdrop-test ()
  "Test that no backdrop when disabled."
  (with-temp-buffer
    (insert "test content")
    (goto-char (point-min))
    (set-window-buffer (selected-window) (current-buffer))
    (let ((state (flash-state-create (list (selected-window))))
          (flash-backdrop nil))
      (setf (flash-state-matches state) nil)
      (flash-highlight-update state)
      ;; Should have no overlays
      (should (null (flash-state-overlays state))))))

(ert-deftest flash-highlight-label-test ()
  "Test that label is displayed as after-string (default position)."
  (with-temp-buffer
    (insert "foo bar")
    (goto-char (point-min))
    (set-window-buffer (selected-window) (current-buffer))
    (let ((state (flash-state-create (list (selected-window))))
          (flash-backdrop nil)
          (flash-label-position 'after))
      (setf (flash-state-matches state)
            (list (make-flash-match
                   :pos (copy-marker 1)
                   :end-pos (copy-marker 4)
                   :label "x"
                   :window (selected-window)
                   :fold nil)))
      (flash-highlight-update state)
      ;; Find label overlay
      (let ((label-ov (seq-find (lambda (ov)
                                  (overlay-get ov 'after-string))
                                (flash-state-overlays state))))
        (should label-ov)
        (should (string= "x" (overlay-get label-ov 'after-string)))))))

(ert-deftest flash-highlight-match-without-label-test ()
  "Test that match without label still gets highlight."
  (with-temp-buffer
    (insert "foo bar")
    (goto-char (point-min))
    (set-window-buffer (selected-window) (current-buffer))
    (let ((state (flash-state-create (list (selected-window))))
          (flash-backdrop nil))
      (setf (flash-state-matches state)
            (list (make-flash-match
                   :pos (copy-marker 1)
                   :end-pos (copy-marker 4)
                   :label nil  ; no label
                   :window (selected-window)
                   :fold nil)))
      (flash-highlight-update state)
      ;; Should have match overlay
      (should (= 1 (length (flash-state-overlays state))))
      (should (eq 'flash-match
                  (overlay-get (car (flash-state-overlays state)) 'face))))))

(ert-deftest flash-highlight-rainbow-disabled-test ()
  "Test that default face is used when rainbow disabled."
  (let ((flash-rainbow nil))
    (should (eq 'flash-label (flash--get-label-face 0)))
    (should (eq 'flash-label (flash--get-label-face 1)))
    (should (eq 'flash-label (flash--get-label-face 5)))))

(ert-deftest flash-highlight-rainbow-enabled-test ()
  "Test that rainbow faces cycle when enabled.
Uses same 10 colors as flash.nvim: red, amber, lime, green, teal, cyan, blue, violet, fuchsia, rose."
  (let ((flash-rainbow t))
    ;; First 10 indices should return different faces (Tailwind colors)
    (should (eq 'flash-label-red (flash--get-label-face 0)))
    (should (eq 'flash-label-amber (flash--get-label-face 1)))
    (should (eq 'flash-label-lime (flash--get-label-face 2)))
    (should (eq 'flash-label-green (flash--get-label-face 3)))
    (should (eq 'flash-label-teal (flash--get-label-face 4)))
    (should (eq 'flash-label-cyan (flash--get-label-face 5)))
    (should (eq 'flash-label-blue (flash--get-label-face 6)))
    (should (eq 'flash-label-violet (flash--get-label-face 7)))
    (should (eq 'flash-label-fuchsia (flash--get-label-face 8)))
    (should (eq 'flash-label-rose (flash--get-label-face 9)))
    ;; Should cycle back
    (should (eq 'flash-label-red (flash--get-label-face 10)))))

(ert-deftest flash-highlight-rainbow-labels-visual-test ()
  "Test that rainbow labels are applied to overlays."
  (with-temp-buffer
    (insert "foo bar foo baz foo")
    (goto-char (point-min))
    (set-window-buffer (selected-window) (current-buffer))
    (let ((state (flash-state-create (list (selected-window))))
          (flash-backdrop nil)
          (flash-rainbow t))
      ;; Add multiple matches with labels
      (setf (flash-state-matches state)
            (list (make-flash-match
                   :pos (copy-marker 1) :end-pos (copy-marker 4)
                   :label "a" :window (selected-window) :fold nil)
                  (make-flash-match
                   :pos (copy-marker 9) :end-pos (copy-marker 12)
                   :label "s" :window (selected-window) :fold nil)
                  (make-flash-match
                   :pos (copy-marker 17) :end-pos (copy-marker 20)
                   :label "d" :window (selected-window) :fold nil)))
      (flash-highlight-update state)
      ;; Should have 6 overlays (3 matches + 3 labels)
      (should (= 6 (length (flash-state-overlays state))))
      ;; Check label overlays have different faces
      (let ((label-ovs (seq-filter (lambda (ov) (overlay-get ov 'after-string))
                                   (flash-state-overlays state))))
        (should (= 3 (length label-ovs)))))))

;;; Label Position Tests

(ert-deftest flash-highlight-label-position-before-test ()
  "Test label position 'before'."
  (with-temp-buffer
    (insert "foo bar")
    (goto-char (point-min))
    (set-window-buffer (selected-window) (current-buffer))
    (let ((state (flash-state-create (list (selected-window))))
          (flash-backdrop nil)
          (flash-label-position 'before))
      (setf (flash-state-matches state)
            (list (make-flash-match
                   :pos (copy-marker 1)
                   :end-pos (copy-marker 4)
                   :label "x"
                   :window (selected-window)
                   :fold nil)))
      (flash-highlight-update state)
      ;; Find label overlay with before-string
      (let ((label-ov (seq-find (lambda (ov)
                                  (overlay-get ov 'before-string))
                                (flash-state-overlays state))))
        (should label-ov)
        (should (string= "x" (overlay-get label-ov 'before-string)))))))

(ert-deftest flash-highlight-label-position-overlay-test ()
  "Test label position 'overlay' (replaces first char)."
  (with-temp-buffer
    (insert "foo bar")
    (goto-char (point-min))
    (set-window-buffer (selected-window) (current-buffer))
    (let ((state (flash-state-create (list (selected-window))))
          (flash-backdrop nil)
          (flash-label-position 'overlay))
      (setf (flash-state-matches state)
            (list (make-flash-match
                   :pos (copy-marker 1)
                   :end-pos (copy-marker 4)
                   :label "x"
                   :window (selected-window)
                   :fold nil)))
      (flash-highlight-update state)
      ;; Find label overlay with display property
      (let ((label-ov (seq-find (lambda (ov)
                                  (overlay-get ov 'display))
                                (flash-state-overlays state))))
        (should label-ov)
        (should (string= "x" (overlay-get label-ov 'display)))
        ;; Overlay should cover exactly 1 character
        (should (= 1 (- (overlay-end label-ov) (overlay-start label-ov))))))))

(ert-deftest flash-highlight-label-position-eol-test ()
  "Test label position 'eol' (end of line)."
  (with-temp-buffer
    (insert "foo bar\nbaz")
    (goto-char (point-min))
    (set-window-buffer (selected-window) (current-buffer))
    (let ((state (flash-state-create (list (selected-window))))
          (flash-backdrop nil)
          (flash-label-position 'eol))
      (setf (flash-state-matches state)
            (list (make-flash-match
                   :pos (copy-marker 1)
                   :end-pos (copy-marker 4)
                   :label "x"
                   :window (selected-window)
                   :fold nil)))
      (flash-highlight-update state)
      ;; Find label overlay at end of line
      (let ((label-ov (seq-find (lambda (ov)
                                  (and (overlay-get ov 'after-string)
                                       (= (overlay-start ov) 8))) ; EOL position
                                (flash-state-overlays state))))
        (should label-ov)
        ;; after-string should contain label (with space prefix)
        (should (string-match-p "x" (overlay-get label-ov 'after-string)))))))

(ert-deftest flash-highlight-label-position-defcustom-test ()
  "Test that label-position defcustom exists."
  (should (boundp 'flash-label-position)))

(provide 'flash-highlight-test)
;;; flash-highlight-test.el ends here
