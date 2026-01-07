;;; emacs-flash-highlight-test.el --- Tests for emacs-flash-highlight -*- lexical-binding: t -*-

;;; Commentary:
;; ERT tests for emacs-flash-highlight module.

;;; Code:

(require 'ert)
(require 'emacs-flash-state)
(require 'emacs-flash-highlight)

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
                   :label ?a
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
  "Test that label is displayed as after-string."
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
                   :label ?x
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

(provide 'emacs-flash-highlight-test)
;;; emacs-flash-highlight-test.el ends here
