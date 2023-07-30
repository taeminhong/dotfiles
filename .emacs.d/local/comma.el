;;;###autoload
(defun comma-insert-comma-space ()
  "Insert a comma followed by one space at point."
  (interactive "*")
  (insert ", "))

;;;###autoload
(defun comma-insert-comma (&optional arg)
  "Insert a comma at point.
With prefix argument ARG, insert that many commas."
  (interactive "*p")
  (insert  (make-string (max arg 0) ?,)))

;;;###autoload
(defun comma-insert-two-commas (&optional arg)
  "Insert two commas at point.
With prefix argument ARG, insert twice as many as ARG commas."
  (interactive "*p")
  (insert (make-string (* arg 2) ?,)))

;;;###autoload
(defun comma-silence ()
  "Do nothing.
Useful when stopping Emacs from waiting key sequences."
  (interactive)
  nil)

;;;###autoload
(defun comma-delete-space-following-comma (&optional start end)
  "Delete space following comma in region.
Unless region is active, delete them in the current line."
  (interactive "*")
  (if (region-active-p)
      (setq start (or start (region-beginning))
            end (or end (region-end)))
    (setq start (or start (line-beginning-position))
          end (or end (line-end-position))))
  (replace-string-in-region ", " "," start end))

;;;###autoload
(defun comma-install-keybindings ()
  (interactive)
  (global-unset-key (kbd ","))
  (global-set-key (kbd ", SPC") 'comma-insert-comma-space)
  (global-set-key (kbd ", ,") 'comma-insert-two-commas)
  (global-set-key (kbd ", RET") 'comma-insert-comma)
  (global-set-key (kbd ", C-g") 'comma-insert-comma)
  (global-set-key (kbd ", DEL") 'comma-silence)

  (global-set-key (kbd ", . z") 'undo-fu-only-undo)
  (global-set-key (kbd ", . /") 'undo-fu-only-undo)
  (global-set-key (kbd ", . y") 'undo-fu-only-redo)
  (global-set-key (kbd ", . r") 'undo-fu-only-redo)
  (global-set-key (kbd ", . DEL") 'comma-delete-space-following-comma)

  (global-set-key (kbd ", b b") 'ivy-switch-buffer)

  (global-set-key (kbd ", e e") 'eval-last-sexp)
  (global-set-key (kbd ", e d") 'eval-defun)
  (global-set-key (kbd ", e b") 'eval-buffer)
  (global-set-key (kbd ", e :") 'eval-expression)
  (global-set-key (kbd ", e r") 'eval-region)

  (global-set-key (kbd ", f f") 'counsel-find-file)
  (global-set-key (kbd ", f s") 'save-buffer)
  (global-set-key (kbd ", f w") 'write-file)

  (global-set-key (kbd ", g g") 'avy-goto-char-timer)
  (global-set-key (kbd ", g l") 'goto-line)
  (global-set-key (kbd ", g n") 'next-error)
  (global-set-key (kbd ", g p") 'previous-error)

  (global-set-key (kbd ", h f") 'describe-function)
  (global-set-key (kbd ", h v") 'describe-variable)
  (global-set-key (kbd ", h k") 'describe-key)
  (global-set-key (kbd ", h q") 'help-quit)

  (global-set-key (kbd ", o b") 'ivy-switch-buffer-other-window)
  (global-set-key (kbd ", o f") 'find-file-other-frame)
  (global-set-key (kbd ", o d") 'dired-other-window)

  (global-set-key (kbd ", w c") 'windmove-up)
  (global-set-key (kbd ", w t") 'windmove-down)
  (global-set-key (kbd ", w h") 'windmove-left)
  (global-set-key (kbd ", w h") 'windmove-left)
  (global-set-key (kbd ", w 1") 'delete-other-windows)
  (global-set-key (kbd ", w 0") 'delete-window)
  (global-set-key (kbd ", w -") 'split-window-below)
  (global-set-key (kbd ", w |") 'split-window-right))


;;;###autoload
(defun comma-uninstall-keybindings ()
  (interactive)
  (global-unset-key (kbd ","))
  (global-set-key (kbd ",") 'self-insert-command))
