;;; flash-char-test.el --- Tests for flash-char -*- lexical-binding: t -*-

;;; Commentary:
;; ERT tests for flash-char module.

;;; Code:

(require 'ert)
(require 'flash-char)

;; Variables dynamically bound in tests.
(defvar flash-labels)

;;; Search Tests

(ert-deftest flash-char-search-forward-test ()
  "Test forward character search."
  (with-temp-buffer
    (insert "hello world")
    (goto-char (point-min))
    (let ((positions (flash-char--search ?o t)))
      (should (= 2 (length positions)))
      (should (= 5 (car positions)))    ; first 'o' in "hello"
      (should (= 8 (cadr positions))))))  ; second 'o' in "world"

(ert-deftest flash-char-search-backward-test ()
  "Test backward character search."
  (with-temp-buffer
    (insert "hello world")
    (goto-char (point-max))
    (let ((positions (flash-char--search ?o nil)))
      (should (= 2 (length positions)))
      (should (= 8 (car positions)))    ; closest 'o' (in "world")
      (should (= 5 (cadr positions)))))) ; farther 'o' (in "hello")

(ert-deftest flash-char-search-not-found-test ()
  "Test search returns nil when char not found."
  (with-temp-buffer
    (insert "hello world")
    (goto-char (point-min))
    (let ((positions (flash-char--search ?z t)))
      (should (null positions)))))

(ert-deftest flash-char-search-forward-at-eob-test ()
  "Test forward search at end of buffer does not signal `end-of-buffer'."
  (with-temp-buffer
    (insert "hello")
    (goto-char (point-max))
    (should-not (flash-char--search ?h t))))

(ert-deftest flash-char-search-line-boundary-test ()
  "Test search respects line boundary by default."
  (with-temp-buffer
    (insert "hello\nworld")
    (goto-char (point-min))
    (let ((flash-char-multi-line nil)
          (positions (flash-char--search ?w t)))
      ;; 'w' is on second line, should not be found
      (should (null positions)))))

(ert-deftest flash-char-search-multi-line-test ()
  "Test search crosses lines when multi-line enabled."
  (with-temp-buffer
    (insert "hello\nworld")
    (goto-char (point-min))
    (set-window-buffer (selected-window) (current-buffer))
    (let ((flash-char-multi-line t))
      (let ((positions (flash-char--search ?w t)))
        ;; 'w' is on second line, should be found
        (should (= 1 (length positions)))))))

;;; Create Matches Tests

(ert-deftest flash-char-create-matches-test ()
  "Test creating match structs from positions."
  (with-temp-buffer
    (insert "test")
    (let ((matches (flash-char--create-matches '(1 3))))
      (should (= 2 (length matches)))
      (should (= 1 (marker-position (flash-match-pos (car matches)))))
      (should (= 3 (marker-position (flash-match-pos (cadr matches))))))))

;;; Motion Tests

(ert-deftest flash-char-find-basic-test ()
  "Test basic f motion."
  (with-temp-buffer
    (insert "hello world")
    (goto-char (point-min))
    (set-window-buffer (selected-window) (current-buffer))
    (flash-char-find 1 ?w)
    (should (= 7 (point)))))  ; position of 'w' in "world"

(ert-deftest flash-char-find-to-test ()
  "Test t motion (to before char)."
  (with-temp-buffer
    (insert "hello world")
    (goto-char (point-min))
    (set-window-buffer (selected-window) (current-buffer))
    (flash-char-find-to 1 ?w)
    (should (= 6 (point)))))  ; one before 'w'

(ert-deftest flash-char-find-backward-test ()
  "Test F motion (backward)."
  (with-temp-buffer
    (insert "hello world")
    (goto-char (point-max))
    (set-window-buffer (selected-window) (current-buffer))
    (flash-char-find-backward 1 ?o)
    (should (= 8 (point)))))  ; 'o' in "world"

(ert-deftest flash-char-find-to-backward-test ()
  "Test T motion (backward, to after char)."
  (with-temp-buffer
    (insert "hello world")
    (goto-char (point-max))
    (set-window-buffer (selected-window) (current-buffer))
    (flash-char-find-to-backward 1 ?o)
    (should (= 9 (point)))))  ; one after 'o' in "world"

;;; Repeat Tests

(ert-deftest flash-char-repeat-test ()
  "Test ; repeat in same direction."
  (with-temp-buffer
    (insert "ababa")
    (goto-char (point-min))
    (set-window-buffer (selected-window) (current-buffer))
    ;; First find
    (flash-char-find 1 ?a)
    (should (= 3 (point)))  ; second 'a'
    ;; Repeat
    (flash-char-repeat)
    (should (= 5 (point)))))  ; third 'a'

(ert-deftest flash-char-repeat-reverse-test ()
  "Test , repeat in opposite direction."
  (with-temp-buffer
    (insert "ababa")
    (goto-char (point-min))
    (set-window-buffer (selected-window) (current-buffer))
    ;; First find forward
    (flash-char-find 1 ?a)
    (should (= 3 (point)))  ; second 'a'
    ;; Repeat forward
    (flash-char-repeat)
    (should (= 5 (point)))  ; third 'a'
    ;; Repeat reverse (backward)
    (flash-char-repeat-reverse)
    (should (= 3 (point)))))  ; back to second 'a'

;;; State Storage Tests

(ert-deftest flash-char-stores-last-motion-test ()
  "Test that last motion is stored for repeat."
  (with-temp-buffer
    (insert "hello")
    (goto-char (point-min))
    (set-window-buffer (selected-window) (current-buffer))
    (flash-char-find 1 ?l)
    (should (eq 'find flash-char--last-motion))
    (should (= ?l flash-char--last-char))
    (should flash-char--last-forward)))

(ert-deftest flash-char-stores-last-to-motion-test ()
  "Test that 't' motion is stored correctly."
  (with-temp-buffer
    (insert "hello")
    (goto-char (point-min))
    (set-window-buffer (selected-window) (current-buffer))
    (flash-char-find-to 1 ?l)
    (should (eq 'find-to flash-char--last-motion))))

;;; Evil Repeat Compatibility Tests

(ert-deftest flash-char-negative-count-test ()
  "Test that negative count reverses direction (for evil repeat)."
  (with-temp-buffer
    (insert "ababa")
    (goto-char 3)  ; middle 'a'
    (set-window-buffer (selected-window) (current-buffer))
    ;; Positive count - forward
    (flash-char-find 1 ?a)
    (should (= 5 (point)))  ; last 'a'
    ;; Negative count - backward
    (flash-char-find -1 ?a)
    (should (= 3 (point)))))  ; back to middle 'a'

(ert-deftest flash-char-backward-negative-count-test ()
  "Test that negative count in backward function goes forward."
  (with-temp-buffer
    (insert "ababa")
    (goto-char 3)  ; middle 'a'
    (set-window-buffer (selected-window) (current-buffer))
    ;; Positive count - backward
    (flash-char-find-backward 1 ?a)
    (should (= 1 (point)))  ; first 'a'
    ;; Negative count - forward
    (flash-char-find-backward -1 ?a)
    (should (= 3 (point)))))  ; back to middle 'a'

(ert-deftest flash-char-repeat-after-reverse-test ()
  "Test that ; works correctly after \\ (evil reverse repeat).
Sequence: fa → ; → \\ → ; should continue forward."
  (with-temp-buffer
    (insert "a_a_a_a_a")  ; positions 1, 3, 5, 7, 9
    (goto-char 1)
    (set-window-buffer (selected-window) (current-buffer))
    ;; fa - find first 'a' forward (skips current, goes to next)
    (flash-char-find 1 ?a)
    (should (= 3 (point)))
    ;; ; - repeat forward
    (flash-char-repeat)
    (should (= 5 (point)))
    ;; Simulate evil's \\ (reverse repeat) - calls with negative count
    (flash-char-find -1 ?a)
    (should (= 3 (point)))  ; went backward
    ;; ; - should still go forward (state preserved)
    (flash-char-repeat)
    (should (= 5 (point)))))  ; forward again!

;;; Defcustom Tests

(ert-deftest flash-char-defcustom-jump-labels-test ()
  "Test that jump-labels defcustom exists."
  (should (boundp 'flash-char-jump-labels))
  (should (booleanp flash-char-jump-labels)))

(ert-deftest flash-char-defcustom-multi-line-test ()
  "Test that multi-line defcustom exists."
  (should (boundp 'flash-char-multi-line))
  (should (booleanp flash-char-multi-line)))

(ert-deftest flash-char-defcustom-reserved-labels-test ()
  "Test that reserved-labels defcustom exists."
  (should (boundp 'flash-char-reserved-labels))
  (should (stringp flash-char-reserved-labels)))

(ert-deftest flash-char-filtered-labels-test ()
  "Test that reserved labels are filtered out."
  (let ((flash-labels "asdfgh")
        (flash-char-reserved-labels "ai"))
    (let ((filtered (flash-char--filtered-labels)))
      ;; 'a' and 'i' should be removed (but 'i' not in labels anyway)
      (should-not (string-match-p "a" filtered))
      ;; 's', 'd', 'f', 'g', 'h' should remain
      (should (string-match-p "s" filtered))
      (should (string-match-p "d" filtered))
      (should (= 5 (length filtered))))))

(provide 'flash-char-test)
;;; flash-char-test.el ends here
