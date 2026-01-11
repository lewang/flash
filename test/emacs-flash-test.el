;;; emacs-flash-test.el --- Tests for emacs-flash -*- lexical-binding: t -*-

;;; Commentary:
;; ERT tests for emacs-flash main module.

;;; Code:

(require 'ert)
(require 'emacs-flash)

(ert-deftest emacs-flash-format-prompt-empty-test ()
  "Test prompt formatting with empty pattern."
  (should (string= "Flash: " (emacs-flash--format-prompt "" 0))))

(ert-deftest emacs-flash-format-prompt-with-pattern-test ()
  "Test prompt formatting with pattern."
  (should (string= "Flash [foo] (3): " (emacs-flash--format-prompt "foo" 3))))

(ert-deftest emacs-flash-defcustom-labels-test ()
  "Test that labels defcustom exists and has default value."
  (should (boundp 'emacs-flash-labels))
  (should (stringp emacs-flash-labels))
  (should (> (length emacs-flash-labels) 0)))

(ert-deftest emacs-flash-defcustom-multi-window-test ()
  "Test that multi-window defcustom exists."
  (should (boundp 'emacs-flash-multi-window))
  (should (booleanp emacs-flash-multi-window)))

(ert-deftest emacs-flash-defcustom-autojump-test ()
  "Test that autojump defcustom exists."
  (should (boundp 'emacs-flash-autojump))
  (should (booleanp emacs-flash-autojump)))

(ert-deftest emacs-flash-defcustom-backdrop-test ()
  "Test that backdrop defcustom exists."
  (should (boundp 'emacs-flash-backdrop))
  (should (booleanp emacs-flash-backdrop)))

(ert-deftest emacs-flash-defcustom-case-fold-test ()
  "Test that case-fold defcustom exists."
  (should (boundp 'emacs-flash-case-fold))
  (should (booleanp emacs-flash-case-fold)))

(ert-deftest emacs-flash-jump-command-exists-test ()
  "Test that emacs-flash-jump command exists."
  (should (fboundp 'emacs-flash-jump))
  (should (commandp 'emacs-flash-jump)))

(ert-deftest emacs-flash-defcustom-rainbow-test ()
  "Test that rainbow defcustom exists."
  (should (boundp 'emacs-flash-rainbow))
  (should (booleanp emacs-flash-rainbow)))

(ert-deftest emacs-flash-defcustom-highlight-matches-test ()
  "Test that highlight-matches defcustom exists."
  (should (boundp 'emacs-flash-highlight-matches))
  (should (booleanp emacs-flash-highlight-matches)))

(ert-deftest emacs-flash-defcustom-multi-char-labels-test ()
  "Test that multi-char-labels defcustom exists."
  (should (boundp 'emacs-flash-multi-char-labels))
  (should (booleanp emacs-flash-multi-char-labels)))

;;; Phase 7 defcustom tests

(ert-deftest emacs-flash-defcustom-jumplist-test ()
  "Test that jumplist defcustom exists and defaults to t."
  (should (boundp 'emacs-flash-jumplist))
  (should (eq t emacs-flash-jumplist)))

(ert-deftest emacs-flash-defcustom-search-history-test ()
  "Test that search-history defcustom exists and defaults to nil."
  (should (boundp 'emacs-flash-search-history))
  (should (null emacs-flash-search-history)))

(ert-deftest emacs-flash-defcustom-nohlsearch-test ()
  "Test that nohlsearch defcustom exists and defaults to nil."
  (should (boundp 'emacs-flash-nohlsearch))
  (should (null emacs-flash-nohlsearch)))

(ert-deftest emacs-flash-defcustom-min-pattern-length-test ()
  "Test that min-pattern-length defcustom exists and defaults to 0."
  (should (boundp 'emacs-flash-min-pattern-length))
  (should (= 0 emacs-flash-min-pattern-length)))

(ert-deftest emacs-flash-last-pattern-var-test ()
  "Test that last-pattern variable exists."
  (should (boundp 'emacs-flash--last-pattern)))

(ert-deftest emacs-flash-jump-continue-exists-test ()
  "Test that emacs-flash-jump-continue command exists."
  (should (fboundp 'emacs-flash-jump-continue))
  (should (commandp 'emacs-flash-jump-continue)))

(ert-deftest emacs-flash-multi-window-search-test ()
  "Test search works across multiple windows with different buffers."
  (let ((buf1 (generate-new-buffer "*flash-test-1*"))
        (buf2 (generate-new-buffer "*flash-test-2*"))
        (orig-window (selected-window))
        win1 win2)
    (unwind-protect
        (progn
          ;; Setup two windows with different buffers
          (set-window-buffer orig-window buf1)
          (with-current-buffer buf1
            (insert "hello world hello"))
          (setq win1 orig-window)

          (setq win2 (split-window-right))
          (set-window-buffer win2 buf2)
          (with-current-buffer buf2
            (insert "hello there"))

          ;; Create state with both windows
          (let ((state (emacs-flash-state-create (list win1 win2))))
            (setf (emacs-flash-state-pattern state) "hello")
            (emacs-flash-search state)

            ;; Should find matches in both buffers (2 in buf1, 1 in buf2)
            (should (= 3 (length (emacs-flash-state-matches state))))

            ;; Check that markers point to correct buffers
            (dolist (match (emacs-flash-state-matches state))
              (let ((marker (emacs-flash-match-pos match))
                    (win (emacs-flash-match-window match)))
                (should (markerp marker))
                (should (eq (marker-buffer marker) (window-buffer win)))))))

      ;; Cleanup
      (delete-window win2)
      (kill-buffer buf1)
      (kill-buffer buf2))))

(ert-deftest emacs-flash-multi-window-highlight-test ()
  "Test highlighting works across multiple windows with different buffers."
  (let ((buf1 (generate-new-buffer "*flash-test-1*"))
        (buf2 (generate-new-buffer "*flash-test-2*"))
        (orig-window (selected-window))
        win1 win2)
    (unwind-protect
        (progn
          ;; Setup two windows with different buffers
          (set-window-buffer orig-window buf1)
          (with-current-buffer buf1
            (insert "test content"))
          (setq win1 orig-window)

          (setq win2 (split-window-right))
          (set-window-buffer win2 buf2)
          (with-current-buffer buf2
            (insert "test data"))

          ;; Create state, search, and highlight
          (let ((state (emacs-flash-state-create (list win1 win2))))
            (setf (emacs-flash-state-pattern state) "test")
            (emacs-flash-search state)
            (emacs-flash-label-matches state)

            ;; This should not error - the bug was "Marker points into wrong buffer"
            (emacs-flash-highlight-update state)

            ;; Should have overlays in both buffers
            (let ((overlays (emacs-flash-state-overlays state)))
              (should (> (length overlays) 0))
              ;; Cleanup overlays
              (emacs-flash-highlight-clear state))))

      ;; Cleanup
      (delete-window win2)
      (kill-buffer buf1)
      (kill-buffer buf2))))

(provide 'emacs-flash-test)
;;; emacs-flash-test.el ends here
