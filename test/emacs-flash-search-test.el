;;; emacs-flash-search-test.el --- Tests for emacs-flash-search -*- lexical-binding: t -*-

;;; Commentary:
;; ERT tests for emacs-flash-search module.

;;; Code:

(require 'ert)
(require 'emacs-flash-state)
(require 'emacs-flash-search)

(ert-deftest emacs-flash-search-empty-pattern-test ()
  "Test search with empty pattern returns no matches."
  (with-temp-buffer
    (insert "foo bar baz")
    (let ((state (emacs-flash-state-create (list (selected-window)))))
      (setf (emacs-flash-state-pattern state) "")
      (emacs-flash-search state)
      (should (null (emacs-flash-state-matches state))))))

(ert-deftest emacs-flash-search-basic-test ()
  "Test basic search finds matches."
  (with-temp-buffer
    (insert "foo bar foo baz foo")
    (goto-char (point-min))
    ;; Need to display buffer in window for window-start/end to work
    (set-window-buffer (selected-window) (current-buffer))
    (let ((state (emacs-flash-state-create (list (selected-window)))))
      (setf (emacs-flash-state-pattern state) "foo")
      (emacs-flash-search state)
      (should (= 3 (length (emacs-flash-state-matches state))))
      ;; Check first match
      (let ((first-match (car (emacs-flash-state-matches state))))
        (should (= 1 (marker-position (emacs-flash-match-pos first-match))))
        (should (= 4 (marker-position (emacs-flash-match-end-pos first-match))))
        (should (eq (selected-window) (emacs-flash-match-window first-match)))))))

(ert-deftest emacs-flash-search-case-insensitive-test ()
  "Test search is case-insensitive."
  (with-temp-buffer
    (insert "Foo FOO foo")
    (goto-char (point-min))
    (set-window-buffer (selected-window) (current-buffer))
    (let ((state (emacs-flash-state-create (list (selected-window)))))
      (setf (emacs-flash-state-pattern state) "foo")
      (emacs-flash-search state)
      (should (= 3 (length (emacs-flash-state-matches state)))))))

(ert-deftest emacs-flash-search-no-match-test ()
  "Test search with no matches."
  (with-temp-buffer
    (insert "foo bar baz")
    (goto-char (point-min))
    (set-window-buffer (selected-window) (current-buffer))
    (let ((state (emacs-flash-state-create (list (selected-window)))))
      (setf (emacs-flash-state-pattern state) "xyz")
      (emacs-flash-search state)
      (should (null (emacs-flash-state-matches state))))))

(ert-deftest emacs-flash-search-markers-test ()
  "Test that matches use markers."
  (with-temp-buffer
    (insert "test")
    (goto-char (point-min))
    (set-window-buffer (selected-window) (current-buffer))
    (let ((state (emacs-flash-state-create (list (selected-window)))))
      (setf (emacs-flash-state-pattern state) "test")
      (emacs-flash-search state)
      (let ((match (car (emacs-flash-state-matches state))))
        (should (markerp (emacs-flash-match-pos match)))
        (should (markerp (emacs-flash-match-end-pos match)))))))

(ert-deftest emacs-flash-get-fold-at-visible-test ()
  "Test fold detection returns nil for visible text."
  (with-temp-buffer
    (insert "visible text")
    (should (null (emacs-flash--get-fold-at 5)))))

(provide 'emacs-flash-search-test)
;;; emacs-flash-search-test.el ends here
