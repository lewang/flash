;;; emacs-flash-state-test.el --- Tests for emacs-flash-state -*- lexical-binding: t -*-

;;; Commentary:
;; ERT tests for emacs-flash-state module.

;;; Code:

(require 'ert)
(require 'emacs-flash-state)

;;; emacs-flash-state tests

(ert-deftest emacs-flash-state-create-test ()
  "Test state creation with defaults."
  (let ((state (emacs-flash-state-create)))
    (should (emacs-flash-state-p state))
    (should (string= "" (emacs-flash-state-pattern state)))
    (should (null (emacs-flash-state-matches state)))
    (should (null (emacs-flash-state-overlays state)))
    (should (null (emacs-flash-state-target state)))
    (should (eq (selected-window) (emacs-flash-state-start-window state)))
    (should (= (point) (emacs-flash-state-start-point state)))
    (should (equal (list (selected-window)) (emacs-flash-state-windows state)))))

(ert-deftest emacs-flash-state-create-with-windows-test ()
  "Test state creation with custom windows."
  (let* ((wins (window-list))
         (state (emacs-flash-state-create wins)))
    (should (equal wins (emacs-flash-state-windows state)))))

(ert-deftest emacs-flash-state-cleanup-test ()
  "Test that cleanup removes overlays."
  (let ((state (emacs-flash-state-create)))
    ;; Add some overlays
    (let ((ov1 (make-overlay (point-min) (point-min)))
          (ov2 (make-overlay (point-min) (point-min))))
      (setf (emacs-flash-state-overlays state) (list ov1 ov2))
      ;; Verify overlays exist
      (should (overlay-buffer ov1))
      (should (overlay-buffer ov2))
      ;; Cleanup
      (emacs-flash-state-cleanup state)
      ;; Verify overlays are deleted
      (should-not (overlay-buffer ov1))
      (should-not (overlay-buffer ov2)))))

;;; emacs-flash-match tests

(ert-deftest emacs-flash-match-create-test ()
  "Test match creation."
  (let ((match (make-emacs-flash-match
                :pos (point-marker)
                :end-pos (point-marker)
                :label ?a
                :window (selected-window)
                :fold nil)))
    (should (emacs-flash-match-p match))
    (should (markerp (emacs-flash-match-pos match)))
    (should (eq ?a (emacs-flash-match-label match)))
    (should (null (emacs-flash-match-fold match)))))

(ert-deftest emacs-flash-match-with-fold-test ()
  "Test match with fold field."
  (let ((match (make-emacs-flash-match
                :pos (point-marker)
                :end-pos (point-marker)
                :label ?b
                :window (selected-window)
                :fold 10)))
    (should (= 10 (emacs-flash-match-fold match)))))

(provide 'emacs-flash-state-test)
;;; emacs-flash-state-test.el ends here
