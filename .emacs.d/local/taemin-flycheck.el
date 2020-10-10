;;; package taemin-flycheck.el --- customizing flycheck

(require 'flycheck)

(defun taemin-flycheck--select-error-list-window ()
  (select-window (get-buffer-window flycheck-error-list-buffer)))

(defun taemin-flycheck-select-after-list-errors (select)
  (if select
      (advice-add 'flycheck-list-errors :after #'taemin-flycheck--select-error-list-window)
    (advice-remove 'flycheck-list-errors #'taemin-flycheck--select-error-list-window)))

(defun taemin-flycheck-change-keymap-prefix (prefix)
  (define-key flycheck-mode-map flycheck-keymap-prefix nil)
  (setq flycheck-keymap-prefix prefix)
  (define-key flycheck-mode-map flycheck-keymap-prefix flycheck-command-map))

(provide 'taemin-flycheck)
