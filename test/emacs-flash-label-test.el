;;; emacs-flash-label-test.el --- Tests for emacs-flash-label -*- lexical-binding: t -*-

;;; Commentary:
;; ERT tests for emacs-flash-label module.

;;; Code:

(require 'ert)
(require 'emacs-flash-state)
(require 'emacs-flash-label)

(ert-deftest emacs-flash-label-assigns-labels-test ()
  "Test that labels are assigned to matches."
  (with-temp-buffer
    (insert "foo bar foo")
    (goto-char (point-min))
    (set-window-buffer (selected-window) (current-buffer))
    (let ((state (emacs-flash-state-create (list (selected-window)))))
      (setf (emacs-flash-state-pattern state) "foo")
      ;; Add matches manually
      (setf (emacs-flash-state-matches state)
            (list (make-emacs-flash-match
                   :pos (copy-marker 1)
                   :end-pos (copy-marker 4)
                   :label nil
                   :window (selected-window)
                   :fold nil)
                  (make-emacs-flash-match
                   :pos (copy-marker 9)
                   :end-pos (copy-marker 12)
                   :label nil
                   :window (selected-window)
                   :fold nil)))
      (emacs-flash-label-matches state)
      ;; Both should have labels
      (should (emacs-flash-match-label (car (emacs-flash-state-matches state))))
      (should (emacs-flash-match-label (cadr (emacs-flash-state-matches state))))
      ;; Labels should be different
      (should-not (eq (emacs-flash-match-label (car (emacs-flash-state-matches state)))
                      (emacs-flash-match-label (cadr (emacs-flash-state-matches state))))))))

(ert-deftest emacs-flash-label-sorted-by-distance-test ()
  "Test that closer matches get earlier labels."
  (with-temp-buffer
    (insert "foo bar foo baz foo")
    (goto-char 10)  ; Position cursor in middle
    (set-window-buffer (selected-window) (current-buffer))
    (let ((state (emacs-flash-state-create (list (selected-window)))))
      (setf (emacs-flash-state-pattern state) "foo")
      (setf (emacs-flash-state-start-point state) 10)
      ;; Add matches at positions 1, 9, 17
      (setf (emacs-flash-state-matches state)
            (list (make-emacs-flash-match
                   :pos (copy-marker 1)
                   :end-pos (copy-marker 4)
                   :label nil
                   :window (selected-window)
                   :fold nil)
                  (make-emacs-flash-match
                   :pos (copy-marker 9)
                   :end-pos (copy-marker 12)
                   :label nil
                   :window (selected-window)
                   :fold nil)
                  (make-emacs-flash-match
                   :pos (copy-marker 17)
                   :end-pos (copy-marker 20)
                   :label nil
                   :window (selected-window)
                   :fold nil)))
      (emacs-flash-label-matches state)
      ;; Match at pos 9 (closest to 10) should have first label 'a'
      (let ((match-at-9 (cl-find 9 (emacs-flash-state-matches state)
                                 :key (lambda (m) (marker-position (emacs-flash-match-pos m))))))
        (should (eq ?a (emacs-flash-match-label match-at-9)))))))

(ert-deftest emacs-flash-label-conflict-detection-test ()
  "Test that conflicting labels are skipped."
  (with-temp-buffer
    (insert "ab abc abd")
    (goto-char (point-min))
    (set-window-buffer (selected-window) (current-buffer))
    (let ((state (emacs-flash-state-create (list (selected-window)))))
      (setf (emacs-flash-state-pattern state) "ab")
      ;; 'c' and 'd' would conflict (abc, abd exist)
      (let ((labels (emacs-flash--available-labels state "ab")))
        ;; 'c' and 'd' should not be in available labels
        (should-not (memq ?c labels))
        (should-not (memq ?d labels))
        ;; 'a' should be available (aba doesn't exist)
        (should (memq ?a labels))))))

(ert-deftest emacs-flash-label-empty-pattern-test ()
  "Test that all labels available for empty pattern."
  (with-temp-buffer
    (insert "test")
    (set-window-buffer (selected-window) (current-buffer))
    (let ((state (emacs-flash-state-create (list (selected-window)))))
      (let ((labels (emacs-flash--available-labels state "")))
        ;; All labels should be available
        (should (= (length labels) (length emacs-flash-labels)))))))

(ert-deftest emacs-flash-find-match-by-label-test ()
  "Test finding match by label."
  (with-temp-buffer
    (insert "foo bar")
    (set-window-buffer (selected-window) (current-buffer))
    (let ((state (emacs-flash-state-create (list (selected-window)))))
      (setf (emacs-flash-state-matches state)
            (list (make-emacs-flash-match
                   :pos (copy-marker 1)
                   :end-pos (copy-marker 4)
                   :label ?x
                   :window (selected-window)
                   :fold nil)
                  (make-emacs-flash-match
                   :pos (copy-marker 5)
                   :end-pos (copy-marker 8)
                   :label ?y
                   :window (selected-window)
                   :fold nil)))
      ;; Find match with label 'x'
      (let ((match (emacs-flash-find-match-by-label state ?x)))
        (should match)
        (should (= 1 (marker-position (emacs-flash-match-pos match)))))
      ;; Find match with label 'y'
      (let ((match (emacs-flash-find-match-by-label state ?y)))
        (should match)
        (should (= 5 (marker-position (emacs-flash-match-pos match)))))
      ;; Non-existent label returns nil
      (should-not (emacs-flash-find-match-by-label state ?z)))))

(ert-deftest emacs-flash-label-no-matches-test ()
  "Test labeling with no matches."
  (with-temp-buffer
    (insert "test")
    (set-window-buffer (selected-window) (current-buffer))
    (let ((state (emacs-flash-state-create (list (selected-window)))))
      (setf (emacs-flash-state-matches state) nil)
      ;; Should not error
      (emacs-flash-label-matches state)
      (should (null (emacs-flash-state-matches state))))))

(ert-deftest emacs-flash-label-more-matches-than-labels-test ()
  "Test when there are more matches than labels."
  (with-temp-buffer
    (insert (make-string 100 ?x))  ; 100 x's
    (goto-char (point-min))
    (set-window-buffer (selected-window) (current-buffer))
    (let* ((emacs-flash-labels "ab")  ; Only 2 labels
           (state (emacs-flash-state-create (list (selected-window)))))
      ;; Create 5 matches
      (setf (emacs-flash-state-matches state)
            (cl-loop for i from 1 to 5
                     collect (make-emacs-flash-match
                              :pos (copy-marker (* i 10))
                              :end-pos (copy-marker (1+ (* i 10)))
                              :label nil
                              :window (selected-window)
                              :fold nil)))
      (emacs-flash-label-matches state)
      ;; Only first 2 should have labels
      (let ((labeled (cl-count-if #'emacs-flash-match-label
                                  (emacs-flash-state-matches state))))
        (should (= 2 labeled))))))

(provide 'emacs-flash-label-test)
;;; emacs-flash-label-test.el ends here
