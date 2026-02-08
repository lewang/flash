;;; flash-evil-test.el --- Tests for flash-evil -*- lexical-binding: t -*-

;;; Commentary:
;; ERT tests for flash-evil module.
;; These tests require evil-mode to be available.

;;; Code:

(require 'ert)

(declare-function evil-get-command-property "evil")

;; Only run tests if evil is available
(when (require 'evil nil t)
  (require 'flash)
  (require 'flash-evil)

  (ert-deftest flash-evil-jump-defined-test ()
    "Test that flash-evil-jump is defined as evil motion."
    (should (fboundp 'flash-evil-jump))
    (should (evil-get-command-property 'flash-evil-jump :type)))

  (ert-deftest flash-evil-jump-type-test ()
    "Test that flash-evil-jump has inclusive type."
    (should (eq 'inclusive
                (evil-get-command-property 'flash-evil-jump :type))))

  (ert-deftest flash-evil-jump-is-jump-test ()
    "Test that flash-evil-jump is marked as jump."
    (should (evil-get-command-property 'flash-evil-jump :jump)))

  (ert-deftest flash-evil-setup-defined-test ()
    "Test that flash-evil-setup is defined."
    (should (fboundp 'flash-evil-setup))
    (should (commandp 'flash-evil-setup)))

)

(provide 'flash-evil-test)
;;; flash-evil-test.el ends here
