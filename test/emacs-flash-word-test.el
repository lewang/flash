;;; emacs-flash-word-test.el --- Tests for emacs-flash-word -*- lexical-binding: t -*-

;;; Commentary:
;; ERT tests for emacs-flash-word module.

;;; Code:

(require 'ert)
(require 'emacs-flash-state)
(require 'emacs-flash-word)

(ert-deftest emacs-flash-word-collect-words-test ()
  "Test that word mode collects all visible words."
  (with-temp-buffer
    (insert "one two three four five")
    (goto-char (point-min))
    (set-window-buffer (selected-window) (current-buffer))
    (let ((state (emacs-flash-state-create (list (selected-window)))))
      (emacs-flash-word--collect-words state)
      ;; Should have 5 matches (one per word)
      (should (= 5 (length (emacs-flash-state-matches state))))
      ;; First match should be at position 1
      (should (= 1 (marker-position
                    (emacs-flash-match-pos
                     (car (emacs-flash-state-matches state)))))))))

(ert-deftest emacs-flash-word-matches-at-word-start-test ()
  "Test that matches are at word beginnings."
  (with-temp-buffer
    (insert "hello world test")
    (goto-char (point-min))
    (set-window-buffer (selected-window) (current-buffer))
    (let ((state (emacs-flash-state-create (list (selected-window)))))
      (emacs-flash-word--collect-words state)
      ;; Check positions: "hello"=1, "world"=7, "test"=13
      (let ((positions (mapcar (lambda (m)
                                 (marker-position (emacs-flash-match-pos m)))
                               (emacs-flash-state-matches state))))
        (should (member 1 positions))
        (should (member 7 positions))
        (should (member 13 positions))))))

(ert-deftest emacs-flash-word-empty-buffer-test ()
  "Test word mode with empty buffer."
  (with-temp-buffer
    (set-window-buffer (selected-window) (current-buffer))
    (let ((state (emacs-flash-state-create (list (selected-window)))))
      (emacs-flash-word--collect-words state)
      ;; Empty buffer has no words
      (should (= 0 (length (emacs-flash-state-matches state)))))))

(ert-deftest emacs-flash-word-single-word-test ()
  "Test word mode with single word buffer."
  (with-temp-buffer
    (insert "single")
    (goto-char (point-min))
    (set-window-buffer (selected-window) (current-buffer))
    (let ((state (emacs-flash-state-create (list (selected-window)))))
      (emacs-flash-word--collect-words state)
      ;; Should have exactly 1 match
      (should (= 1 (length (emacs-flash-state-matches state)))))))

(ert-deftest emacs-flash-word-command-exists-test ()
  "Test that word jump command exists."
  (should (fboundp 'emacs-flash-word-jump)))

(ert-deftest emacs-flash-word-labels-assigned-test ()
  "Test that labels are assigned to word matches."
  (with-temp-buffer
    (insert "one two three")
    (goto-char (point-min))
    (set-window-buffer (selected-window) (current-buffer))
    (let ((state (emacs-flash-state-create (list (selected-window)))))
      (emacs-flash-word--collect-words state)
      (emacs-flash-label-matches state)
      ;; All matches should have labels
      (dolist (match (emacs-flash-state-matches state))
        (should (emacs-flash-match-label match))))))

(ert-deftest emacs-flash-word-punctuation-test ()
  "Test that punctuation separates words correctly."
  (with-temp-buffer
    (insert "foo,bar.baz;qux")
    (goto-char (point-min))
    (set-window-buffer (selected-window) (current-buffer))
    (let ((state (emacs-flash-state-create (list (selected-window)))))
      (emacs-flash-word--collect-words state)
      ;; Should have 4 words
      (should (= 4 (length (emacs-flash-state-matches state)))))))

(ert-deftest emacs-flash-word-multiline-test ()
  "Test that word mode works across lines."
  (with-temp-buffer
    (insert "first line\nsecond line\nthird")
    (goto-char (point-min))
    (set-window-buffer (selected-window) (current-buffer))
    (let ((state (emacs-flash-state-create (list (selected-window)))))
      (emacs-flash-word--collect-words state)
      ;; Should have 5 words: first, line, second, line, third
      (should (= 5 (length (emacs-flash-state-matches state)))))))

(provide 'emacs-flash-word-test)
;;; emacs-flash-word-test.el ends here
