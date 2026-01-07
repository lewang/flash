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

(provide 'emacs-flash-test)
;;; emacs-flash-test.el ends here
