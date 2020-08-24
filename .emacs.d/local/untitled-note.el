(defconst untitled-note-directory
  (file-name-as-directory
   (expand-file-name "untitled-note" user-emacs-directory)))

(defvar untitled-note-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x C-w") #'untitled-note-write-file)
    map)
  "Keymap for `untitled-note-mode'")

(define-minor-mode untitled-note-mode
  "Untitled note mode."
  nil
  " untitled-note"
  '()
  (when untitled-note-mode
    (cd (getenv "PWD"))))

(defun untitled-note--cleanup ()
  (when (and untitled-note-mode buffer-file-name)
    (untitled-note--delete-file-and-backup buffer-file-name)))

(defun untitled-note--delete-file-and-backup (file-name)
  (delete-file file-name)
  (delete-file (make-backup-file-name file-name)))

(defun untitled-note-new-note ()
  "Create a new note"
  (interactive)
  (switch-to-buffer (generate-new-buffer "untitled"))
  (let ((name (buffer-name))
        (inhibit-message t)
        (temporary-file-directory untitled-note-directory))
    (write-file
     (make-temp-file
      (format "%s." (file-name-nondirectory (getenv "PWD")))))
    (rename-buffer name)
    (untitled-note-mode 1)))

(defun untitled-note-garbage-collect ()
  "Delete old temporary files in `untitled-note-directory'"
  (interactive)
  (shell-command (format "find %s -type f -atime +30 -delete"
                         (shell-quote-argument untitled-note-directory))))

(defun untitled-note-save-all (&optional arg)
  (save-some-buffers
   t
   (lambda () (and buffer-file-name untitled-note-mode))))

(defun untitled-note-write-file ()
  "Write contents into the other file and remove the temporary file"
  (interactive)
  (let ((old-file-name buffer-file-name))
    (call-interactively 'write-file)
    (unless (string= buffer-file-name old-file-name)
      (untitled-note--delete-file-and-backup old-file-name))))

(eval-after-load 'recentf
  '(add-to-list 'recentf-exclude untitled-note-directory))
(make-directory untitled-note-directory t)
(advice-add 'save-buffers-kill-terminal :before #'untitled-note-save-all)
(add-hook 'kill-buffer-hook #'untitled-note--cleanup)

(provide 'untitled-note)
