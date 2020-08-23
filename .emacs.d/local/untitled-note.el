(defconst untitled-note-directory
  (file-name-as-directory
   (expand-file-name "untitled-note" user-emacs-directory)))

(define-minor-mode untitled-note-mode
  "Untitled note mode."
  nil
  " untitled-note"
  '()
  (when untitled-note-mode
    (cd (getenv "PWD"))))

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

;; exclude untitled note files from the recentf list
(eval-after-load 'recentf
  '(add-to-list 'recentf-exclude untitled-note-directory))

(make-directory untitled-note-directory t)

(advice-add 'save-buffers-kill-terminal :before #'untitled-note-save-all)

(provide 'untitled-note)
