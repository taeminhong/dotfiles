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
  :init-value nil
  :lighter " Untitled"
  :keymap '()
  :after-hook (when untitled-note-mode
                (cd (getenv "PWD"))))

(defun untitled-note--cleanup ()
  (when (and untitled-note-mode buffer-file-name)
    (untitled-note--delete-files buffer-file-name
                                 (make-backup-file-name buffer-file-name)
                                 buffer-auto-save-file-name)))

(defun untitled-note--delete-files (&rest file-names)
  (ignore-errors
    (mapcar 'delete-file file-names)))

(defun untitled-note-generate-new-buffer (name)
  "Create and return a untitled-note buffer with a name based on NAME."
  (let ((buf (generate-new-buffer name)))
    (with-current-buffer buf
      (let ((name (buffer-name))
            (inhibit-message t)
            (temporary-file-directory untitled-note-directory))
        (write-file
         (make-temp-file
          (format "%s." (file-name-nondirectory (getenv "PWD")))
          nil
          ".txt"))
        (rename-buffer name)
        (untitled-note-mode 1)
        (visual-line-mode 1)))
    buf))

(defun untitled-note-new-note ()
  "Create a new note"
  (interactive)
  (switch-to-buffer (untitled-note-generate-new-buffer "untitled")))

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
  (let ((old-file-name buffer-file-name)
        (old-auto-save-file-name buffer-auto-save-file-name))
    (call-interactively 'write-file)
    (unless (string= buffer-file-name old-file-name)
      (untitled-note--delete-files old-file-name
                                   (make-backup-file-name old-file-name)
                                   old-auto-save-file-name)
      ;; When the major mode for the new file is the same as the old one
      ;; (probably text-mode), untitled-note mode may remain turned on.
      ;; Turn it off forcibly.
      (untitled-note-mode -1))))

(eval-after-load 'recentf
  '(add-to-list 'recentf-exclude untitled-note-directory))
(make-directory untitled-note-directory t)
(advice-add 'save-buffers-kill-terminal :before #'untitled-note-save-all)
(add-hook 'kill-buffer-hook #'untitled-note--cleanup)

(provide 'untitled-note)
