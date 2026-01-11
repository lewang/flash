;;; emacs-flash-treesitter-test.el --- Tests for emacs-flash-treesitter -*- lexical-binding: t -*-

;;; Commentary:
;; ERT tests for emacs-flash-treesitter module.
;; Full tests require Emacs 29+ with treesitter support.

;;; Code:

(require 'ert)
(require 'emacs-flash-treesitter)

;;; Basic function existence tests

(ert-deftest emacs-flash-treesitter-command-exists-test ()
  "Test that emacs-flash-treesitter command is defined."
  (should (fboundp 'emacs-flash-treesitter))
  (should (commandp 'emacs-flash-treesitter)))

(ert-deftest emacs-flash-treesitter-evil-command-exists-test ()
  "Test that emacs-flash-treesitter-evil command is defined."
  (should (fboundp 'emacs-flash-treesitter-evil))
  (should (commandp 'emacs-flash-treesitter-evil)))

(ert-deftest emacs-flash-treesitter-available-p-test ()
  "Test treesitter availability check function."
  (should (fboundp 'emacs-flash-treesitter--available-p))
  ;; Should return boolean
  (should (or (eq t (emacs-flash-treesitter--available-p))
              (eq nil (emacs-flash-treesitter--available-p)))))

(ert-deftest emacs-flash-treesitter-buffer-has-parser-p-test ()
  "Test buffer parser check function."
  (should (fboundp 'emacs-flash-treesitter--buffer-has-parser-p))
  ;; In temp buffer without parser, should return nil
  (with-temp-buffer
    (should (null (emacs-flash-treesitter--buffer-has-parser-p)))))

(ert-deftest emacs-flash-treesitter-get-nodes-test ()
  "Test node collection function exists."
  (should (fboundp 'emacs-flash-treesitter--get-nodes-at-point)))

(ert-deftest emacs-flash-treesitter-nodes-to-matches-test ()
  "Test node to match conversion."
  (with-temp-buffer
    (insert "0123456789ABCDEFGHIJ")  ; 20 chars
    ;; Empty list
    (should (null (emacs-flash-treesitter--nodes-to-matches nil)))
    ;; Single node
    (let ((nodes '((1 10 "function" 0))))
      (let ((matches (emacs-flash-treesitter--nodes-to-matches nodes)))
        (should (= 1 (length matches)))
        (let ((match (car matches)))
          (should (= 1 (marker-position (emacs-flash-match-pos match))))
          (should (= 10 (marker-position (emacs-flash-match-end-pos match))))
          (should (equal "a" (emacs-flash-match-label match))))))))

(ert-deftest emacs-flash-treesitter-nodes-to-matches-multiple-test ()
  "Test multiple nodes get different labels."
  (with-temp-buffer
    (insert (make-string 50 ?x))  ; 50 chars
    (let ((nodes '((1 10 "function" 0)
                   (1 20 "class" 1)
                   (1 30 "module" 2))))
      (let ((matches (emacs-flash-treesitter--nodes-to-matches nodes)))
        (should (= 3 (length matches)))
        ;; Each should have different label
        (should (equal "a" (emacs-flash-match-label (nth 0 matches))))
        (should (equal "s" (emacs-flash-match-label (nth 1 matches))))
        (should (equal "d" (emacs-flash-match-label (nth 2 matches))))))))

(ert-deftest emacs-flash-treesitter-defcustom-test ()
  "Test that customization options are defined."
  (should (boundp 'emacs-flash-treesitter-max-depth))
  (should (integerp emacs-flash-treesitter-max-depth)))

(provide 'emacs-flash-treesitter-test)
;;; emacs-flash-treesitter-test.el ends here
