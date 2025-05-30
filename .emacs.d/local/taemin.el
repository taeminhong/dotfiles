(require 'misc)
(require 'diff)
(require 'etags)
(require 'man)
(require 'compile)
(require 'subr-x)
(require 'term)
(require 'dash)

(defmacro taemin-bound-and-true-p (var)
  "Return the value of symbol VAR if it is bound, else nil."
  `(and (boundp (quote ,var)) ,var))

(defvar taemin-terminal-kill-on-exit t)

(defvar taemin-terminal-sentinel-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" (lambda () (interactive) (quit-window t)))
    map))

(defun taemin--terminal-sentinel (proc msg)
  (let ((buffer (process-buffer proc)))
    (if (and (eq buffer (current-buffer))
             taemin-terminal-kill-on-exit)
        (quit-window t)
      (with-current-buffer buffer
        (use-local-map taemin-terminal-sentinel-map)))))

(defvar taemin-makefile-regex-alist
  '(("^make" . "^[Mm]akefile\\'")
    ("^cabal" . "\\.cabal\\'")))

(defun taemin--terminal-active-buffer-p (buf)
  "Return non-nil if given buffer is a kind of term and the process
is alive."
  (with-current-buffer buf
    (and (derived-mode-p 'term-mode)
         (term-check-proc buf))))

(defun taemin-electric-pair-conservative-inhibit (char)
  "Customized version of `electric-pair-pair-conservative-inhibit'"
  (or
   ;; I find it more often preferable not to pair when the
   ;; same char is next.
   (eq char (char-after))
   ;; Don't pair up when we insert the second of "" or """
   (and (eq char ?\")
        (eq char (char-before))
	(eq char (char-before (1- (point)))))
   ;; It is preferable not to pair next to a word.
   (eq (char-syntax (following-char)) ?w)
   ;; Don't pair " at the end of a word.
   (and (eq char ?\")
        (> (point) 2)
        (eq (char-syntax (char-before (1- (point)))) ?w))))

(defun taemin--compile-command (read)
  (let ((command (eval compile-command)))
   (if (or compilation-read-command read)
       (compilation-read-command command)
     command)))

(defun taemin--select-buffer-window (buf)
  (select-window
   (get-buffer-window buf))
  buf)

(defun taemin-select-window-after (command select)
  (if select
      (advice-add command :filter-return #'taemin--select-buffer-window)
    (advice-remove command #'taemin--select-buffer-window)))

(defun taemin-select-window-after-compilation (select)
  (taemin-select-window-after 'compilation-start select))

(defun taemin-select-window-after-man (select)
  (taemin-select-window-after 'Man-getpage-in-background select))

(defun taemin-compile (command &optional comint directory)
  "Compile from the DIRECTORY and select the compilation window.
Instead of a string, DIRECTORY can be a function taking a one string argument
and returning a directory.
The function will be called with a command string.
If the function returns nil, buffer's default-directory will be used."
  (interactive
   (list
    (taemin--compile-command current-prefix-arg)
    (consp current-prefix-arg)))
  (unless (equal command (eval compile-command))
    (setq compile-command command))
  (save-some-buffers (not compilation-ask-about-save)
                     compilation-save-buffers-predicate)
  (with-temp-buffer
    (cd (or (if (functionp directory)
                (funcall directory command)
              directory)
            default-directory))
    (setq-default compilation-directory default-directory)
    (compilation-start command comint)))

(defun taemin-locate-project-directory (path)
  (cl-some (lambda (name) (locate-dominating-file path name))
           '(".git" ".svn" ".hg" ".projectile")))

(defun taemin-project-compile (command &optional comint)
  "Compile from the project root directory and select the compilation window.
The concept of a project root directory is pretty simple - just a
folder containing VCS repo (e.g. git) or .projectile file in it.
No project root directory found, then this compiles from the
buffer's default-directory."
  (interactive
   (list
    (taemin--compile-command current-prefix-arg)
    (consp current-prefix-arg)))
  (taemin-compile command
                  comint
                  (lambda (_)
                    (taemin-locate-project-directory default-directory))))

(defun taemin-assoc-makefile-regex (command)
  (cl-some (lambda (pair)
             (if (string-match-p (car pair) (string-trim command))
                 pair))
           taemin-makefile-regex-alist))

(defun taemin-locate-makefile-directory (command)
  (let ((regex (cdr (taemin-assoc-makefile-regex command))))
    (locate-dominating-file default-directory
                            (lambda (dir)
                              (cl-some 'file-regular-p
                                       (directory-files dir t regex))))))

(defun taemin-makefile-compile (command &optional comint)
  "Compile from the directory containig a makefile of the give command"
  (interactive
   (list
    (taemin--compile-command current-prefix-arg)
    (consp current-prefix-arg)))
  (taemin-compile command
                  comint
                  'taemin-locate-makefile-directory))

(defun taemin--read-file-name-default (fmt dir default-filename mustmatch initial predicate)
  (let ((basename (file-name-nondirectory default-filename)))
    (if (taemin-bound-and-true-p ivy-mode)
        (read-file-name (format fmt "")
                        (file-name-directory default-filename)
                        nil
                        mustmatch
                        basename
                        predicate)
      (read-file-name (format fmt
                              (concat "(default " basename ")"))
                      dir
                      default-filename
                      mustmatch
                      initial
                      predicate))))

(defun taemin-diff (old new &optional switches no-async)
  "Find and display the differences between OLD and NEW files.
When called interactively, read NEW, then OLD, using the
minibuffer.  The default for NEW is the current buffer's file
name, and the default for OLD is a backup file for NEW, if one
exists.  If NO-ASYNC is non-nil, call diff synchronously.

When called interactively with a prefix argument, prompt
interactively for diff switches.  Otherwise, the switches
specified in the variable `diff-switches' are passed to the diff command."
  (interactive
   (let* ((newf (if (and buffer-file-name (file-exists-p buffer-file-name))
                    (taemin--read-file-name-default "Diff new file %s:"
                                                    nil buffer-file-name t nil nil)
		  (read-file-name "Diff new file: " nil nil t)))
          (oldf (file-newest-backup newf)))
     (setq oldf (if (and oldf (file-exists-p oldf))
		    (taemin--read-file-name-default "Diff original file %s:"
                                                    (file-name-directory oldf)
                                                    oldf t nil nil)
		  (read-file-name "Diff original file: "
				  (file-name-directory newf) nil t)))
     (list oldf newf (diff-switches))))
  (select-window
   (display-buffer
    (diff-no-select old new switches no-async))))

(defun taemin-visit-tags-table (file &optional local)
  "Tell tags commands to use tags table file FILE.
FILE should be the name of a file created with the `etags' program.
A directory name is ok too; it means file TAGS in that directory.

This is a `visit-tags-table' wrapper for a better file name completion with ivy"
  (interactive (list (taemin--read-file-name-default
                      "Visit tags table %s: "
                      default-directory
                      (expand-file-name "TAGS" default-directory)
                      t nil nil)
		     current-prefix-arg))
  (visit-tags-table file local))

(defun taemin-revert-buffer ()
  "Revert buffer without being prompted and preserving modes"
  (interactive)
  (revert-buffer (not current-prefix-arg) t t))

(defun taemin-advice-haskell-load-prompt (orig-fun &rest args)
  "Prompt target on starting REPL"
  (let ((haskell-process-load-or-reload-prompt t))
    (apply orig-fun args)))

(defun taemin-man-no-completion (man-args)
  "The same as `man' but without completion."
  (interactive
   (list (let* ((default-entry (Man-default-man-entry))
		(input (read-from-minibuffer
			(format "Manual entry%s"
				(if (string= default-entry "")
				    ": "
				  (format " (default %s): " default-entry))))))
	   (if (string= input "")
	       (error "No man args given")
	     input))))
  ;; Possibly translate the "subject(section)" syntax into the
  ;; "section subject" syntax and possibly downcase the section.
  (setq man-args (Man-translate-references man-args))
  (Man-getpage-in-background man-args))

(defun taemin-show-init-time ()
  (message "init completeted in %f seconds"
	   (float-time (time-subtract after-init-time
				      before-init-time))))

(defun taemin-create-buffer-file-parent-directories ()
  (when buffer-file-name
    (let ((dir (file-name-directory buffer-file-name)))
      (unless (file-exists-p dir)
        (make-directory dir t)))))

(defun taemin--escape-double-quotes (str)
  "Return STR with every double-quote escaped with backslash."
  (save-match-data
    (replace-regexp-in-string "\"" "\\\\\"" str)))

(defun taemin-touch (file-name)
  "Change file access and modification times.
If the given file doesn't exist, it is created with default permissions."
  (interactive (list (read-file-name "File: " default-directory)))
  (shell-command (format "touch \"%s\""
                         (taemin--escape-double-quotes file-name))
                 current-prefix-arg
                 shell-command-default-error-buffer))

(defun taemin-kill-this-buffer-no-prompt ()
  "Kill this buffer without prompt."
  (interactive)
  (set-buffer-modified-p nil)
  (kill-this-buffer))

(defun taemin-occur (regexp &optional nlines)
  "Run `occur' respecting `search-whitespace-regexp' variable.
Spaces in REGEXP will be replaced by `search-whitespace-regexp'
if it's non-nil. NLINES has the same meaning as in `occur'."
  (interactive
   (occur-read-primary-args))
  (let ((search-spaces-regexp
	 (if (if isearch-regexp
		 isearch-regexp-lax-whitespace
	       isearch-lax-whitespace)
	     search-whitespace-regexp)))
    (occur (if isearch-regexp-function
	       (propertize regexp
			   'isearch-string isearch-string
			   'isearch-regexp-function-descr
                           (isearch--describe-regexp-mode isearch-regexp-function))
	     regexp)
	   nlines
	   (if (use-region-p) (region-bounds)))))

(defun taemin-terminal (&optional other-window)
  "The same as `ansi-term', but doesn't ask which shell to use and
picks \"terminal\" for the buffer name. The buffer will appear in
another window if OTHER-WINDOW is non nil."
  (interactive)
  (let* ((buffer-name (generate-new-buffer-name "*terminal*"))
         (buffer (get-buffer-create buffer-name)))
    (if other-window
        (switch-to-buffer-other-window buffer)
      (switch-to-buffer buffer))
    (term-mode)
    (term-exec buffer buffer-name shell-file-name nil nil)
    (term-char-mode)
    (let (term-escape-char)
      (term-set-escape-char ?\C-x))))

(defun taemin-terminal-other-window ()
  "Like `taemin-terminal', but create a terminal in other window."
  (interactive)
  (taemin-terminal t))

(defun taemin-terminal-other-window-reuse ()
  "Like `taemin-terminal', but create a new window or reuses an
existing one. If there is already an active terminal buffer,
select that buffer in another window."
  (interactive)
  (let ((terminal-buffers (-filter #'taemin--terminal-active-buffer-p
                                   (buffer-list (current-buffer)))))
    (if terminal-buffers
        (switch-to-buffer-other-window (car terminal-buffers))
      (taemin-terminal t))))

(advice-add 'term-sentinel :after #'taemin--terminal-sentinel)

(provide 'taemin)
