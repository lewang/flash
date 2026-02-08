;;; flash-treesitter-test.el --- Tests for flash-treesitter -*- lexical-binding: t -*-

;;; Commentary:
;; ERT tests for flash-treesitter module.
;; Full tests require Emacs 29+ with treesitter support.

;;; Code:

(require 'ert)
(require 'flash-treesitter)

;;; Basic function existence tests

(ert-deftest flash-treesitter-command-exists-test ()
  "Test that flash-treesitter command is defined."
  (should (fboundp 'flash-treesitter))
  (should (commandp 'flash-treesitter)))

(ert-deftest flash-treesitter-evil-command-exists-test ()
  "Test that flash-treesitter-evil command is defined."
  (should (fboundp 'flash-treesitter-evil))
  (should (commandp 'flash-treesitter-evil)))

(ert-deftest flash-treesitter-available-p-test ()
  "Test treesitter availability check function."
  (should (fboundp 'flash-treesitter--available-p))
  ;; Should return boolean
  (should (or (eq t (flash-treesitter--available-p))
              (eq nil (flash-treesitter--available-p)))))

(ert-deftest flash-treesitter-buffer-has-parser-p-test ()
  "Test buffer parser check function."
  (should (fboundp 'flash-treesitter--buffer-has-parser-p))
  ;; In temp buffer without parser, should return nil
  (with-temp-buffer
    (should (null (flash-treesitter--buffer-has-parser-p)))))

(ert-deftest flash-treesitter-get-nodes-test ()
  "Test node collection function exists."
  (should (fboundp 'flash-treesitter--get-nodes-at-point)))

(ert-deftest flash-treesitter-nodes-to-matches-test ()
  "Test node to match conversion."
  (with-temp-buffer
    (insert "0123456789ABCDEFGHIJ")  ; 20 chars
    ;; Empty list
    (should (null (flash-treesitter--nodes-to-matches nil)))
    ;; Single node
    (let ((nodes '((1 10 "function" 0))))
      (let ((matches (flash-treesitter--nodes-to-matches nodes)))
        (should (= 1 (length matches)))
        (let ((match (car matches)))
          (should (= 1 (marker-position (flash-match-pos match))))
          (should (= 10 (marker-position (flash-match-end-pos match))))
          (should (equal "a" (flash-match-label match))))))))

(ert-deftest flash-treesitter-nodes-to-matches-multiple-test ()
  "Test multiple nodes get different labels."
  (with-temp-buffer
    (insert (make-string 50 ?x))  ; 50 chars
    (let ((nodes '((1 10 "function" 0)
                   (1 20 "class" 1)
                   (1 30 "module" 2))))
      (let ((matches (flash-treesitter--nodes-to-matches nodes)))
        (should (= 3 (length matches)))
        ;; Each should have different label
        (should (equal "a" (flash-match-label (nth 0 matches))))
        (should (equal "s" (flash-match-label (nth 1 matches))))
        (should (equal "d" (flash-match-label (nth 2 matches))))))))

(ert-deftest flash-treesitter-defcustom-test ()
  "Test that customization options are defined."
  (should (boundp 'flash-treesitter-max-depth))
  (should (integerp flash-treesitter-max-depth)))

(provide 'flash-treesitter-test)
;;; flash-treesitter-test.el ends here
