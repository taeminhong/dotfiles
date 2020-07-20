(require 'misc)
(require 'diff)
(require 'etags)

(defmacro taemin-bound-and-true-p (var)
  "Return the value of symbol VAR if it is bound, else nil."
  `(and (boundp (quote ,var)) ,var))

(defvar taemin-makefile-regex-alist
      '(("make" . "^\\(Makefile\\|makefile\\)$")
        ("cabal" . "\\.cabal$")))

(defconst taemin--note-paper-file-prefix "TNP")

(defun taemin--do-forward-word (direction line-boundary limit select)
  (if (= (point) line-boundary)
      (when (/= line-boundary limit)
        (goto-char (+ line-boundary direction)))
    (forward-word direction)
    (goto-char (funcall select (point) line-boundary))))

(defun taemin-forward-word (&optional n)
  (interactive "^p")
  (let ((limit          (if (< 0 n) (point-max) (point-min)))
        (direction      (if (< 0 n) 1 -1))
        (select         (if (< 0 n) #'min #'max))
        (line-boundary  (if (< 0 n) (line-end-position) (line-beginning-position)))
        (inc            (if (< 0 n) -1 1)))
    (while (and (/= n 0)
                (/= (point) limit))
      (taemin--do-forward-word direction line-boundary limit select)
      (setq n (+ n inc)))))

(defun taemin-backward-word (&optional n)
  (interactive "^p")
  (taemin-forward-word (- n)))

(defun taemin--do-kill-word (direction line-end limit skip-chars skip-syntax)
  (let (whitespace-end
        non-word-end ;; non-word-consistuent characters
        word-beg)
    ;; whitespace, non-word, and word can be obtained
    ;; by PCRE (\s*)([^\s\w]*)\W*(\w*)
    (save-excursion
      (funcall skip-chars "[:space:]" line-end)
      (setq whitespace-end (point))
      (save-excursion
        (funcall skip-syntax "^w" line-end)
        (setq word-beg (point)))
      (funcall skip-chars "^[:space:]" word-beg)
      (setq non-word-end (point)))
    ;; (point) <= whitespace-end <= non-word-end <= word-beg <= line-end
    ;; if forward direction, otherwise
    ;; line-end <= word-beg <= non-word-end <= whitespace-end <= (point)
    (let ((compare (if (= direction 1) '<= '>=)))
      (cl-assert (funcall compare (point) whitespace-end))
      (cl-assert (funcall compare whitespace-end non-word-end))
      (cl-assert (funcall compare non-word-end word-beg))
      (cl-assert (funcall compare word-beg line-end)))
    (cond ((= (point) line-end)
           ;; kill newline character
           (when (/= line-end limit)
             (kill-region (point)
                          (+ (point) direction))))
          ((or (= line-end whitespace-end)
               (< 1
                  (abs (- (point) whitespace-end))))
           ;; kill whitespaces only
           (kill-region (point) whitespace-end))
          ((= whitespace-end word-beg)
           (kill-word direction))
          (t
           (kill-region (point) non-word-end)))))

(defun taemin-kill-word (n)
  (interactive "^p")
  (let ((direction      (if (< 0 n) 1 -1))
        (line-end       (if (< 0 n) (line-end-position) (line-beginning-position)))
        (limit          (if (< 0 n) (point-max) (point-min)))
        (skip-chars     (if (< 0 n) #'skip-chars-forward #'skip-chars-backward))
        (skip-syntax    (if (< 0 n) #'skip-syntax-forward #'skip-syntax-backward))
        (inc            (if (< 0 n) -1 1)))
    (while (and (/= 0 n)
                (/= (point) limit))
      (taemin--do-kill-word direction line-end limit skip-chars skip-syntax)
      (setq n (+ n inc)))))

(defun taemin-backward-kill-word (n)
  (interactive "^p")
  (taemin-kill-word (- n)))

(defun taemin--make-range (beg end)
  (cons beg end))

(defun taemin--range-beginning (range)
  (car range))

(defun taemin--range-end (range)
  (cdr range))

(defun taemin--nearby-defun (pos)
  (let (end)
    (save-excursion
      (goto-char pos)
      (end-of-defun)
      (setq end (point))
      (beginning-of-defun)
      (taemin--make-range (point) end))))

(defun taemin-next-defun (n)
  (interactive "^p")
  (cond ((< n 0)
         (beginning-of-defun (- n)))
        ((> n 0)
         (or (not (eq this-command 'taemin-next-defun))
             (eq last-command 'taemin-next-defun)
             (and transient-mark-mode mark-active)
             (push-mark))
         (taemin-next-defun-iter (point) n))))

(defun taemin-next-defun-iter (start-point n)
  (cl-assert (<= start-point (point)))
  (cond ((<= n 0)
         (point))
        ((= start-point (point-max))
         (signal 'end-of-buffer start-point))
        (t
         (let ((nearby (taemin--nearby-defun (point))))
           (cond ((> (taemin--range-beginning nearby) start-point)
                  (goto-char (taemin--range-beginning nearby))
                  (taemin-next-defun-iter (taemin--range-beginning nearby) (- n 1)))
                 ((> (taemin--range-end nearby) (point))
                  (goto-char (taemin--range-end nearby))
                  (taemin-next-defun-iter start-point n))
                 (t
                  (goto-char (point-max))
                  (taemin-next-defun-iter (point-max) (- n 1))))))))

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

(defun taemin--end-of-defun-spaces (n)
  (cl-assert (> n 0))
  (progn (dotimes (_ n)
           (let ((old-point (point)))
             (end-of-defun)
             (let ((beg (save-excursion (beginning-of-defun-comments) (point))))
               (if (< old-point beg)
                   (goto-char beg))))
           (skip-chars-forward "[:space:]\n")
           (skip-chars-backward "[:space:]"))))

(defun taemin--expand-defun-mark (arg beg end)
  (cond ((> arg 0)
         (taemin--end-of-defun-spaces arg)
         (setq end (point))
         (push-mark beg nil t)
         (goto-char end))
        (t
         (goto-char beg)
         ;; beginning-of-defun behaves
         ;; strange with zero arg - see
         ;; https://lists.gnu.org/r/bug-gnu-emacs/2017-02/msg00196.html
         (unless (= arg -1)
           (beginning-of-defun (1- (- arg))))
         (push-mark end nil t))))

(defun taemin--turning-back-mark-defun-p ()
  (and (region-active-p)
       (= (point) (mark))
       (eq this-command last-command)
       (member last-command '(taemin-mark-defun-back taemin-mark-defun))))

(defun taemin--do-mark-defun (arg)
  (let ((opoint (point))
        beg end)
    (push-mark opoint)
    ;; Try first in this order for the sake of languages with nested
    ;; functions where several can end at the same place as with the
    ;; offside rule, e.g. Python.
    (beginning-of-defun-comments)
    (setq beg (point))
    (taemin--end-of-defun-spaces 1)
    (setq end (point))
    (when (or (and (<= (point) opoint)
                   (> arg 0))
              (= beg (point-min))) ; we were before the first defun!
      ;; beginning-of-defun moved back one defun so we got the wrong
      ;; one.  If ARG < 0, however, we actually want to go back.
      (goto-char opoint)
      (taemin--end-of-defun-spaces 1)
      (setq end (point))
      (beginning-of-defun-comments)
      (setq beg (point)))
    (goto-char beg)
    (taemin--expand-defun-mark arg beg end)))

(defun taemin--append-defun-region (arg)
  (if (> arg 0)
      (taemin--end-of-defun-spaces arg)
    (beginning-of-defun-comments (- arg))))

(defun taemin--mark-defun-interactive (arg)
  (defun reset-arg-and-this-command (n)
    (setq arg n)
    (setq this-command (cond ((> n 0) 'taemin-mark-defun)
                             ((< n 0) 'taemin-mark-defun-back)
                             (t 'taemin-mark-defun-zero))))
  (if (eq last-command 'taemin-mark-defun-back)
      (reset-arg-and-this-command (- arg))
    (reset-arg-and-this-command arg))
  (cond ((= arg 0))
        ((region-active-p)
         (cond ((member last-command '(taemin-mark-defun taemin-mark-defun-back))
                (taemin--append-defun-region arg))
               ((= (point) (mark))
                (taemin--do-mark-defun arg))
               (t
                (when (< (point) (mark))
                  (reset-arg-and-this-command (- arg)))
                (taemin--append-defun-region arg))))
        (t
         (taemin--do-mark-defun arg))))

(defun taemin--mark-defun--non-interactive (arg)
  (cond ((= arg 0))
        ((use-region-p) (taemin--append-defun-region arg))
        (t (taemin--do-mark-defun arg))))

(defun taemin-mark-defun (&optional arg)
  "Put mark at beginning of this defun, point at beginning of next defun.
The defun marked is the one that contains point or follows point.
With positive ARG, mark this and that many next defuns; with negative
ARG, change the direction of marking.

If the mark is active, it marks the next or previous defun(s) after
the one(s) already marked."
  (interactive "p")
  (setq arg (or arg 1))
  (if (called-interactively-p 'any)
      (taemin--mark-defun-interactive arg)
    (taemin--mark-defun--non-interactive arg)))

(defun taemin--beginning-of-line-p (pos)
  (save-excursion
    (goto-char pos)
    (= pos (line-beginning-position))))

(defun taemin--full-line-region-p ()
  (and (use-region-p)
       (taemin--beginning-of-line-p (region-beginning))
       (or (taemin--beginning-of-line-p (region-end))
           (= (region-end) (point-max)))))

(defun taemin--do-mark-line (n)
  (cond ((> n 0)
         (beginning-of-line)
         (push-mark (point) nil t)
         (forward-line n))
        ((< n 0)
         (forward-line 1)
         (push-mark (point) nil t)
         (taemin--forward-line n))))

(defun taemin--span-to-line-boundaries ()
  (cl-assert (region-active-p))
  (let ((old-mark (mark))
        (old-point (point)))
    (cond ((< old-point old-mark)
           (goto-char old-mark)
           (unless (= old-mark (line-beginning-position))
             (forward-line))
           (set-mark (point))
           (goto-char old-point)
           (beginning-of-line))
          (t
           (goto-char old-mark)
           (set-mark (line-beginning-position))
           (goto-char old-point)
           (unless (= old-point (line-beginning-position))
             (forward-line))))))

(defun taemin--forward-line (n)
  (cond ((= n 0) 0)
        ((> n 0) (forward-line n))
        ((= (point) (line-beginning-position)) (forward-line n))
        (t (forward-line (+ n 1)))))

(defun taemin--expand-line-region (n)
  (cl-assert (not (= n 0)))
  (cl-assert (use-region-p))
  (if (taemin--full-line-region-p)
      (taemin--forward-line n)
    (when (and (< (count-lines (mark) (point)) 2)
               (not (taemin--same-sign-p n (- (point) (mark)))))
      (exchange-point-and-mark)
      (cl-assert (taemin--same-sign-p n (- (point) (mark)))))
    (taemin--span-to-line-boundaries)
    (taemin--forward-line
     (if (not (taemin--same-sign-p n (- (point) (mark))))
         n
       (taemin--damp-to-zero n)))))

(defun taemin--same-sign-p (a b &optional exclude-zero)
  (if exclude-zero
      (> (* a b) 0)
    (>= (* a b) 0)))

(defun taemin--damp-to-zero (n &optional delta)
  (setq delta (or delta 1))
  (cond ((> n 0) (- n 1))
        ((< n 0) (+ n 1))
        (t n)))

(defun taemin--mark-line-interactive (arg)
  (defun reset-arg-and-this-command (n)
    (setq arg n)
    (setq this-command
          (cond ((< n 0) 'taemin-mark-line-back)
                ((> n 0) 'taemin-mark-line)
                (t 'taemin-mark-zero))))
  (if (eq last-command 'taemin-mark-line-back)
      (reset-arg-and-this-command (- arg))
    (reset-arg-and-this-command arg))
  (cond ((= arg 0))
        ((region-active-p)
         (cond ((member last-command '(taemin-mark-line taemin-mark-line-back))
                (taemin--forward-line arg))
               ((= (point) (mark))
                (taemin--do-mark-line arg))
               (t
                (when (< (point) (mark))
                  (reset-arg-and-this-command (- arg)))
                (taemin--expand-line-region arg))))
        (t
         (taemin--do-mark-line arg))))

(defun taemin--mark-line-non-interactive (arg)
  (cond ((= arg 0))
        ((use-region-p) (taemin--expand-line-region arg))
        (t (taemin--do-mark-line arg))))

(defun taemin-mark-line (&optional n)
  (interactive "p")
  (setq n (or n 1))
  (if (called-interactively-p 'any)
      (taemin--mark-line-interactive n)
    (taemin--mark-line-non-interactive n)))

(define-minor-mode taemin-note-paper-mode
  "Note paper mode."
  nil
  " Note-paper"
  '()
  (when taemin-note-paper-mode
    (cd (getenv "PWD"))))

(defun taemin-new-note-paper ()
  "Create a note-paper buffer"
  (interactive)
  (switch-to-buffer (generate-new-buffer "untitled note"))
  (let ((name (buffer-name))
        (inhibit-message t))
    (write-file (make-temp-file taemin--note-paper-file-prefix))
    (rename-buffer name)
    (taemin-note-paper-mode 1)))

(defun taemin--compile-command (read)
  (let ((command (eval compile-command)))
   (if (or compilation-read-command read)
       (compilation-read-command command)
     command)))

(defun taemin--select-buffer-window (buf)
  (select-window
   (get-buffer-window buf))
  buf)

(defun taemin-select-window-after-compilation (select)
  (if select
      (advice-add 'compilation-start :filter-return #'taemin--select-buffer-window)
    (advice-remove 'compilation-start #'taemin--select-buffer-window)))

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
             (let ((regex (concat "^[[:space:]]*" (car pair))))
               (if (string-match-p regex command)
                   pair)))
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

(defun taemin--format-region (fmt beg end sub-from sub-to)
  (let ((text (delete-and-extract-region beg end)))
    (insert (format fmt (substring text sub-from sub-to)))))

(defun taemin-toggle-parens ()
  "Replace enclosing parentheses with square brackets and vice versa"
  (interactive)
  (let* ((old-point (point))
         (beg (save-excursion
                (up-list -1 t t)
                (when (looking-at-p "\"")
                  (up-list -1 t t))
                (point)))
         (end (save-excursion
                (goto-char beg)
                (forward-sexp)
                (point))))
    ;; Delete and paste back a character; nothing will change.
    ;; Doing this because we want the cursor stay even after undo.
    (insert (delete-and-extract-region (- (point) 1) (point)))
    (goto-char (- end 1))
    (cond ((looking-at-p ")") (taemin--format-region "[%s]" beg end 1 -1))
          ((looking-at-p "]") (taemin--format-region "(%s)" beg end 1 -1)))
    (goto-char old-point)))

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

;; exclude note-paper temporary file from the recentf list
(eval-after-load 'recentf
  '(add-to-list 'recentf-exclude
                (format "%s[[:alnum:]]+" taemin--note-paper-file-prefix)))

(provide 'taemin)
