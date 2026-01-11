;;; emacs-flash-evil-test.el --- Tests for emacs-flash-evil -*- lexical-binding: t -*-

;;; Commentary:
;; ERT tests for emacs-flash-evil module.
;; These tests require evil-mode to be available.

;;; Code:

(require 'ert)

;; Only run tests if evil is available
(when (require 'evil nil t)
  (require 'emacs-flash)
  (require 'emacs-flash-evil)

  (ert-deftest emacs-flash-evil-jump-defined-test ()
    "Test that emacs-flash-evil-jump is defined as evil motion."
    (should (fboundp 'emacs-flash-evil-jump))
    (should (evil-get-command-property 'emacs-flash-evil-jump :type)))

  (ert-deftest emacs-flash-evil-jump-type-test ()
    "Test that emacs-flash-evil-jump has inclusive type."
    (should (eq 'inclusive
                (evil-get-command-property 'emacs-flash-evil-jump :type))))

  (ert-deftest emacs-flash-evil-jump-is-jump-test ()
    "Test that emacs-flash-evil-jump is marked as jump."
    (should (evil-get-command-property 'emacs-flash-evil-jump :jump)))

  (ert-deftest emacs-flash-evil-select-defined-test ()
    "Test that emacs-flash-evil-select is defined."
    (should (fboundp 'emacs-flash-evil-select))
    (should (commandp 'emacs-flash-evil-select)))

  (ert-deftest emacs-flash-evil-setup-defined-test ()
    "Test that emacs-flash-evil-setup is defined."
    (should (fboundp 'emacs-flash-evil-setup))
    (should (commandp 'emacs-flash-evil-setup)))

  ;; Remote mode tests
  (ert-deftest emacs-flash-evil-remote-defined-test ()
    "Test that emacs-flash-evil-remote motion is defined."
    (should (fboundp 'emacs-flash-evil-remote))
    (should (evil-get-command-property 'emacs-flash-evil-remote :type)))

  (ert-deftest emacs-flash-evil-remote-type-test ()
    "Test that emacs-flash-evil-remote has inclusive type."
    (should (eq 'inclusive
                (evil-get-command-property 'emacs-flash-evil-remote :type))))

  (ert-deftest emacs-flash-evil-remote-save-position-test ()
    "Test remote save position function."
    (with-temp-buffer
      (insert "test content here")
      (goto-char 5)
      (emacs-flash-evil-remote--save-position)
      (should (eq emacs-flash-evil-remote--saved-window (selected-window)))
      (should (= emacs-flash-evil-remote--saved-point 5))))

  (ert-deftest emacs-flash-evil-remote-restore-defined-test ()
    "Test remote restore position function is defined."
    (should (fboundp 'emacs-flash-evil-remote--restore-position)))

  (ert-deftest emacs-flash-evil-remote-valid-prefix-defined-test ()
    "Test valid prefix function is defined."
    (should (fboundp 'emacs-flash-evil-remote--valid-prefix-p))))

(provide 'emacs-flash-evil-test)
;;; emacs-flash-evil-test.el ends here
