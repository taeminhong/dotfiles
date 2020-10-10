;;; package taemin-flycheck.el --- customizing flycheck

(require 'flycheck)

(defun taemin-flycheck-change-keymap-prefix (prefix)
  (define-key flycheck-mode-map flycheck-keymap-prefix nil)
  (setq flycheck-keymap-prefix prefix)
  (define-key flycheck-mode-map flycheck-keymap-prefix flycheck-command-map))

(provide 'taemin-flycheck)
