(require 'misc)
(require 'diff)
(require 'etags)
(require 'man)
(require 'compile)
(require 'subr-x)

(defmacro taemin-bound-and-true-p (var)
  "Return the value of symbol VAR if it is bound, else nil."
  `(and (boundp (quote ,var)) ,var))

(defvar taemin-makefile-regex-alist
  '(("^make" . "^[Mm]akefile\\'")
    ("^cabal" . "\\.cabal\\'")))

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

(defun taemin-select-buffer-window (buf)
  (select-window
   (get-buffer-window buf))
  buf)

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
The concept of a project root directory is pretty simple -
just a folder containing VCS repo (e.g. git) or .projectile file in it.
No project root directory found, then this compiles from the buffer's default-directory."
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

(provide 'taemin)
