(require 'window)

(defconst tmacs-env (getenv "TMUX"))

(defun tmacs--window-first-child (window)
  (or (window-top-child window)
      (window-left-child window)))

(defun tmacs--first-window ()
  (let* ((parent (frame-root-window (window-frame)))
         (child (tmacs--window-first-child parent)))
    (while child
      (setq parent child)
      (setq child (tmacs--window-first-child child)))
    parent))

(defun tmacs--last-window ()
  (let* ((parent (frame-root-window (window-frame)))
         (child (window-last-child parent)))
    (while child
      (setq parent child)
      (setq child (window-last-child child)))
    parent))

(defun tmacs--other-pane (count)
  (let ((target (if (> count 0) ":.+" ":.-")))
    (call-process "tmux" nil nil nil "select-pane" "-t" target)))

;;;###autoload
(defun tmacs-other-window-tmux-aware (&optional count portal)
  (interactive)
  (setq count (or count 1))
  (setq portal (or portal (tmacs--last-window)))
  (if (and tmacs-env
           (eq (selected-window) portal))
      (tmacs--other-pane count)
    (other-window count)))

;;;###autoload
(defun tmacs-other-window-tmux-aware-backward ()
  (interactive)
  (tmacs-other-window-tmux-aware -1 (tmacs--first-window)))

;;;###autoload
(defun tmacs-other-window-quiet ()
  "The same as `other-window' but no message when there's no other
window."
  (interactive)
  (other-window 1))

;;;###autoload
(defun tmacs-other-window-quiet-backward ()
  "The same as `tmacs-other-window-quiet' but in reverse order"
  (interactive)
  (other-window -1))

;;;###autoload
(defun tmacs-other-window-backward ()
  "The same as `other-window' but in reverse order"
  (interactive)
  (other-window -1 nil t))

(provide 'tmacs)
