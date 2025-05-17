(require 'cl-lib)

(cl-defstruct sublimey-word-context
  whitespace-end
  non-word-end
  word-beg)

;;;###autoload
(defvar sublimey-word-leap-empty-lines nil)

(defun sublimey--make-range (beg end)
  (cons beg end))

(defun sublimey--range-beginning (range)
  (car range))

(defun sublimey--range-end (range)
  (cdr range))

(defun sublimey--nearby-defun (pos)
  (let (end)
    (save-excursion
      (goto-char pos)
      (end-of-defun)
      (setq end (point))
      (beginning-of-defun)
      (sublimey--make-range (point) end))))

(defun sublimey--do-forward-word (direction line-boundary limit select)
  (if (= (point) line-boundary)
      (when (/= line-boundary limit)
        (goto-char (+ line-boundary direction)))
    (forward-word direction)
    (goto-char (funcall select (point) line-boundary))))

(defun sublimey--lookup-word (direction line-end limit skip-chars skip-syntax)
  (let (whitespace-end
        non-word-end ;; non-word-constituent characters
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
    (make-sublimey-word-context
     :whitespace-end whitespace-end
     :non-word-end non-word-end
     :word-beg word-beg)))

(defun sublimey--end-of-defun-spaces (n)
  (cl-assert (> n 0))
  (progn (dotimes (_ n)
           (let ((old-point (point)))
             (end-of-defun)
             (let ((beg (save-excursion (beginning-of-defun-comments) (point))))
               (if (< old-point beg)
                   (goto-char beg))))
           (skip-chars-forward "[:space:]\n")
           (skip-chars-backward "[:space:]"))))

(defun sublimey--expand-defun-mark (arg beg end)
  (cond ((> arg 0)
         (sublimey--end-of-defun-spaces arg)
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

(defun sublimey--do-mark-defun (arg)
  (let ((opoint (point))
        beg end)
    (push-mark opoint)
    ;; Try first in this order for the sake of languages with nested
    ;; functions where several can end at the same place as with the
    ;; offside rule, e.g. Python.
    (beginning-of-defun-comments)
    (setq beg (point))
    (sublimey--end-of-defun-spaces 1)
    (setq end (point))
    (when (or (and (<= (point) opoint)
                   (> arg 0))
              (= beg (point-min))) ; we were before the first defun!
      ;; beginning-of-defun moved back one defun so we got the wrong
      ;; one.  If ARG < 0, however, we actually want to go back.
      (goto-char opoint)
      (sublimey--end-of-defun-spaces 1)
      (setq end (point))
      (beginning-of-defun-comments)
      (setq beg (point)))
    (goto-char beg)
    (sublimey--expand-defun-mark arg beg end)))

(defun sublimey--append-defun-region (arg)
  (if (> arg 0)
      (sublimey--end-of-defun-spaces arg)
    (beginning-of-defun-comments (- arg))))

(defun sublimey--reset-this-command (n sym)
  (setq this-command
        (if (> n 0)
            sym
          (intern (concat (symbol-name sym) "-back")))))

(defun sublimey--mark-defun-context-aware (arg)
  (cl-assert (/= arg 0))
  (if (eq last-command 'sublimey-mark-defun-back)
      (sublimey--reset-this-command (setq arg (- arg)) 'sublimey-mark-defun)
    (sublimey--reset-this-command arg 'sublimey-mark-defun))
  (cond ((= arg 0))
        ((region-active-p)
         (cond ((member last-command '(sublimey-mark-defun sublimey-mark-defun-back))
                (sublimey--append-defun-region arg))
               ((= (point) (mark))
                (sublimey--do-mark-defun arg))
               (t
                (when (< (point) (mark))
                  (sublimey--reset-this-command (setq arg (- arg)) 'sublimey-mark-defun))
                (sublimey--append-defun-region arg))))
        (t
         (sublimey--do-mark-defun arg))))

(defun sublimey--empty-line-p ()
  (= (line-beginning-position) (line-end-position)))

(defun sublimey--beginning-of-line-p (pos)
  (save-excursion
    (goto-char pos)
    (= pos (line-beginning-position))))

(defun sublimey--full-line-region-p ()
  (and (use-region-p)
       (sublimey--beginning-of-line-p (region-beginning))
       (or (sublimey--beginning-of-line-p (region-end))
           (= (region-end) (point-max)))))

(defun sublimey--do-mark-line (n)
  (cond ((> n 0)
         (beginning-of-line)
         (push-mark (point) nil t)
         (forward-line n))
        ((< n 0)
         (forward-line 1)
         (push-mark (point) nil t)
         (sublimey--forward-line n))))

(defun sublimey--span-to-line-boundaries ()
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

(defun sublimey--forward-line (n)
  (cond ((= n 0) 0)
        ((> n 0) (forward-line n))
        ((= (point) (line-beginning-position)) (forward-line n))
        (t (forward-line (+ n 1)))))

(defun sublimey--expand-line-region (n)
  (cl-assert (not (= n 0)))
  (cl-assert (use-region-p))
  (if (sublimey--full-line-region-p)
      (sublimey--forward-line n)
    (when (and (< (count-lines (mark) (point)) 2)
               (not (sublimey--same-sign-p n (- (point) (mark)))))
      (exchange-point-and-mark)
      (cl-assert (sublimey--same-sign-p n (- (point) (mark)))))
    (sublimey--span-to-line-boundaries)
    (sublimey--forward-line
     (if (not (sublimey--same-sign-p n (- (point) (mark))))
         n
       (sublimey--damp-to-zero n)))))

(defun sublimey--signum (x)
  (cond ((< x 0) -1)
        ((> x 0) 1)
        (t 0)))

(defun sublimey--same-sign-p (a b &optional exclude-zero)
  (if exclude-zero
      (> (* a b) 0)
    (>= (* a b) 0)))

(defun sublimey--damp-to-zero (n &optional delta)
  (setq delta (or delta 1))
  (cond ((> n 0) (- n 1))
        ((< n 0) (+ n 1))
        (t n)))

(defun sublimey--mark-line-context-aware (arg)
  (cl-assert (/= arg 0))
  (if (eq last-command 'sublimey-mark-line-back)
      (sublimey--reset-this-command (setq arg (- arg)) 'sublimey-mark-line)
    (sublimey--reset-this-command arg 'sublimey-mark-line))
  (cond ((= arg 0))
        ((region-active-p)
         (cond ((member last-command '(sublimey-mark-line sublimey-mark-line-back))
                (sublimey--forward-line arg))
               ((= (point) (mark))
                (sublimey--do-mark-line arg))
               (t
                (when (< (point) (mark))
                  (sublimey--reset-this-command (setq arg (- arg)) 'sublimey-mark-line))
                (sublimey--expand-line-region arg))))
        (t
         (sublimey--do-mark-line arg))))

(defun sublimey--mark-paragraph-context-aware (arg)
  (cl-assert (/= arg 0))
  ;; make ARG positive if the point is going to move forward,
  ;; otherwise make it negative.
  (if (eq last-command 'sublimey-mark-paragraph-back)
      (sublimey--reset-this-command (setq arg (- arg)) 'sublimey-mark-paragraph)
    (sublimey--reset-this-command arg 'sublimey-mark-paragraph))
  ;; preserve the current mark direction if any mark exists.
  (when (and (region-active-p)
             (not (member last-command '(sublimey-mark-paragraph sublimey-mark-paragraph-back)))
             (< (point) (mark)))
    (sublimey--reset-this-command (setq arg (- arg)) 'sublimey-mark-paragraph))
  (sublimey--do-mark-paragraph arg t))

(defun sublimey--do-mark-paragraph (arg &optional extend)
  (cl-assert (/= arg 0))
  (cond (mark-active
         (if (sublimey--same-sign-p (- (point) (mark)) arg t)
             (sublimey-forward-paragraph arg)
           (let ((old-point (point)))
             (sublimey-forward-paragraph arg)
             (when (sublimey--same-sign-p (- old-point (mark))
                                          (- (point) (mark))
                                          t)
               (sublimey-forward-paragraph (sublimey--signum arg))
               (sublimey-forward-paragraph (- (sublimey--signum arg)))))))
        (t
         (sublimey-forward-paragraph arg)
         (push-mark (point))
         (sublimey-backward-paragraph arg)
         (exchange-point-and-mark))))

(defun sublimey--next-defun-iter (start-point n)
  (cl-assert (<= start-point (point)))
  (cond ((<= n 0)
         (point))
        ((= start-point (point-max))
         (signal 'end-of-buffer start-point))
        (t
         (let ((nearby (sublimey--nearby-defun (point))))
           (cond ((> (sublimey--range-beginning nearby) start-point)
                  (goto-char (sublimey--range-beginning nearby))
                  (sublimey--next-defun-iter (sublimey--range-beginning nearby) (- n 1)))
                 ((> (sublimey--range-end nearby) (point))
                  (goto-char (sublimey--range-end nearby))
                  (sublimey--next-defun-iter start-point n))
                 (t
                  (goto-char (point-max))
                  (sublimey--next-defun-iter (point-max) (- n 1))))))))

(defun sublimey--format-region (fmt beg end sub-from sub-to)
  (let ((text (delete-and-extract-region beg end)))
    (insert (format fmt (substring text sub-from sub-to)))))

(defun sublimey--kill-empty-lines (direction skip-chars)
  "On empty line, kill all following empty lines, leaving just one.
On isolated empty line, kill that one.
On the end of line followed by a non-empty line, kill newline.
If DIRECTION is -1, kill preceeding empty lines."
  (let* ((old-point (point))
         (from old-point)
         (was-empty (sublimey--empty-line-p))
         (distance (funcall skip-chars "\n"))
         (to (point)))
    (cond ((= distance 0))
          ((= (abs distance) 1)
           (kill-region from to))
          (t
           ;; goto the last empty line.
           (when (not (sublimey--empty-line-p))
             (setq to (- to direction)))
           (unless was-empty
             (setq from (+ from direction)))
           (goto-char old-point)
           (if (= from to)
               (kill-region (point) (+ (point) direction))
             (kill-region from to))))))

;;;###autoload
(defun sublimey-kill-forward-empty-lines ()
  "On empty line, kill all following empty lines, leaving just one.
On isolated empty line, kill that one.
On the end of line followed by a non-empty line, kill newline."
  (interactive)
  (sublimey--kill-empty-lines 1 #'skip-chars-forward))

;;;###autoload
(defun sublimey-kill-backward-empty-lines ()
  "On empty line, kill all preceeding empty lines, leaving just one.
On isolated empty line, kill that one.
On the beginning of line following a non-empty line, join the line to
previous."
  (interactive)
  (sublimey--kill-empty-lines -1 #'skip-chars-backward))

;;;###autoload
(defun sublimey-forward-word (&optional n)
  (interactive "^p")
  (let ((limit          (if (< 0 n) (point-max) (point-min)))
        (direction      (if (< 0 n) 1 -1))
        (skip-chars     (if (< 0 n) #'skip-chars-forward #'skip-chars-backward))
        (skip-syntax    (if (< 0 n) #'skip-syntax-forward #'skip-syntax-backward))
        (select         (if (< 0 n) #'min #'max)))
    (dotimes (i (abs n))
      (let ((line-end  (if (< 0 n) (line-end-position) (line-beginning-position))))
        (unless (= (point) limit)
          (let* ((context (sublimey--lookup-word direction line-end limit skip-chars skip-syntax))
                 (whitespace-end (sublimey-word-context-whitespace-end context))
                 (non-word-end (sublimey-word-context-non-word-end context))
                 (word-beg (sublimey-word-context-word-beg context)))
            (cond ((= (point) line-end)
                   (if (and sublimey-word-leap-empty-lines
                            (sublimey--empty-line-p))
                       (funcall skip-chars "\n")
                     (when (/= line-end limit)
                       (goto-char (+ (point) direction)))))
                  ((= line-end whitespace-end)
                   (goto-char whitespace-end))
                  ((or (= whitespace-end word-beg)
                       (and (= non-word-end word-beg)
                            (/= word-beg line-end)))
                   (forward-word direction))
                  (t
                   (goto-char non-word-end)))))))))

;;;###autoload
(defun sublimey-backward-word (&optional n)
  (interactive "^p")
  (sublimey-forward-word (- n)))

;;;###autoload
(defun sublimey-kill-word (n)
  (interactive "^p")
  (let ((direction      (if (< 0 n) 1 -1))
        (skip-chars     (if (< 0 n) #'skip-chars-forward #'skip-chars-backward))
        (skip-syntax    (if (< 0 n) #'skip-syntax-forward #'skip-syntax-backward)))
    (dotimes (i (abs n))
      ;; We hava to recalculate line-end and limit everytime because we modify
      ;; the buffer by killing a region.
      (let ((line-end (if (< 0 n) (line-end-position) (line-beginning-position)))
            (limit    (if (< 0 n) (point-max) (point-min))))
        (unless (= (point) limit)
          (let* ((context (sublimey--lookup-word direction line-end limit skip-chars skip-syntax))
                 (whitespace-end (sublimey-word-context-whitespace-end context))
                 (non-word-end (sublimey-word-context-non-word-end context))
                 (word-beg (sublimey-word-context-word-beg context)))
            (cond ((= (point) line-end)
                   ;; kill newline character
                   (if sublimey-word-leap-empty-lines
                       (sublimey--kill-empty-lines direction skip-chars)
                     (when (/= line-end limit)
                       (kill-region (point) (+ (point) direction)))))
                  ((or (= line-end whitespace-end)
                       (< 1
                          (abs (- whitespace-end (point)))))
                   ;; kill whitespaces only
                   (kill-region (point) whitespace-end))
                  ((= whitespace-end word-beg)
                   (kill-region (point) (save-excursion
                                          (forward-word direction)
                                          (point))))
                  (t
                   (kill-region (point) non-word-end)))))))))

;;;###autoload
(defun sublimey-backward-kill-word (n)
  (interactive "^p")
  (sublimey-kill-word (- n)))

;;;###autoload
(defun sublimey-backward-kill-line (&optional arg)
  "Kill the current line backward; if the cursor is at the
beginning of the line, kill the preceding newline."
  (interactive "P")
  (let ((n (prefix-numeric-value arg)))
    (cond ((and arg (= n 0))
           (kill-line))
          ((and arg)
           (kill-line (- n)))
          ((= (point) (point-min))
           (signal 'beginning-of-buffer nil))
          ((= (point) (line-beginning-position))
           (kill-region (point) (1- (point))))
          (t
           (kill-line 0)))))

;;;###autoload
(defun sublimey-back-to-indentation-or-beginning-of-line (n)
  "Go back to the indentation. If cursor is already at the
indentation go to the beginning of the line."
  (interactive "p")
  (let ((old-point (point)))
    (if visual-line-mode
        (beginning-of-visual-line n)
      (move-beginning-of-line n))
    (when (bolp)
      ;; try to back to indentation
      (let ((i (save-excursion (back-to-indentation) (point))))
        (unless (= i old-point)
          (goto-char i))))))

;;;###autoload
(defun sublimey-mark-defun (&optional arg)
  "Put mark at beginning of this defun, point at beginning of next defun.
The defun marked is the one that contains point or follows point.
With positive ARG, mark this and that many next defuns; with negative
ARG, change the direction of marking.
If the mark is active, it marks the next or previous defun(s) after
the one(s) already marked."
  (interactive "p")
  (setq arg (or arg 1))
  (cond ((= arg 0)
         (error "Cannot mark zero defuns"))
        ((called-interactively-p 'any)
         (sublimey--mark-defun-context-aware arg))
        ((use-region-p)
         (sublimey--append-defun-region arg))
        (t
         (sublimey--do-mark-defun arg))))

;;;###autoload
(defun sublimey-mark-line (&optional n)
  "Mark at the beginning of the line, and put point at the beginning of
the next line.
If there is a region, extend it to the line boundaries."
  (interactive "p")
  (setq n (or n 1))
  (cond ((= n 0)
         (error "Cannot mark zero lines"))
        ((called-interactively-p 'any)
         (sublimey--mark-line-context-aware n))
        ((use-region-p)
         (sublimey--expand-line-region n))
        (t
         (sublimey--do-mark-line n))))

;;;###autoload
(defun sublimey-mark-paragraph (&optional arg)
  "Mark at the beginning of this paragraph,and put the point
at the end of this paragraph.
The paragraph marked is the one that contains point or follows point."
  (interactive "P")
  (setq arg (prefix-numeric-value arg))
  (cond ((= arg 0)
         (error "Cannot mark zero paragraphs"))
        ((called-interactively-p 'any)
         (sublimey--mark-paragraph-context-aware arg))
        (t
         (sublimey--do-mark-paragraph arg))))

;;;###autoload
(defun sublimey-backward-paragraph (&optional arg)
  (interactive "P")
  (setq arg (prefix-numeric-value arg))
  (cond ((and (> arg 0) (> (point) (point-min)))
         ;; backward-paragraph says that if the first real line of a paragraph
         ;; is preceded by a blank line, the paragraph starts at that blank
         ;; line. But, we don't want to treat the blank line as the beginning of
         ;; paragraph. Let's exclude it.
         (let ((old-point (point)))
           (when (and (not (sublimey--empty-line-p))
                      (= (point) (line-beginning-position))
                      (save-excursion
                        (goto-char (1- (point)))
                        (sublimey--empty-line-p)))
             (goto-char (1- (point))))
           (backward-paragraph arg)
           (when (and (< (point) (point-max))
                      (sublimey--empty-line-p)
                      (or (> (point) (point-min))
                          (> old-point
                             (save-excursion
                               (skip-chars-forward "[:space:]\n")
                               (skip-chars-backward "[:space:]")
                               (point)))))
             (goto-char (1+ (point))))))
        ((< arg 0)
         (backward-paragraph arg))))

;;;###autoload
(defun sublimey-forward-paragraph (&optional arg)
  (interactive "P")
  (setq arg (prefix-numeric-value arg))
  (if (> arg 0)
      (forward-paragraph arg)
    (sublimey-backward-paragraph (- arg))))

;;;###autoload
(defun sublimey-backward-kill-paragraph (arg)
  (interactive "*p")
  (kill-region (point) (progn (sublimey-backward-paragraph arg) (point))))

;;;###autoload
(defun sublimey-kill-paragraph (arg)
  (interactive "*p")
  (if (> arg 0)
      (kill-paragraph arg)
    (sublimey-backward-kill-paragraph (- arg))))

;;;###autoload
(defun sublimey-delete-blank-lines (&optional delete-all)
  "Same as `delete-blank-lines' but if with a prefix key, this doesn't
leave a single blank line."
  (interactive (list current-prefix-arg))
  (delete-blank-lines)
  (when (and delete-all (looking-at-p "^\n"))
    (delete-char 1)))

;;;###autoload
(defun sublimey-next-defun (n)
  (interactive "^p")
  (cond ((< n 0)
         (beginning-of-defun (- n)))
        ((> n 0)
         (or (not (eq this-command 'sublimey-next-defun))
             (eq last-command 'sublimey-next-defun)
             (and transient-mark-mode mark-active)
             (push-mark))
         (sublimey--next-defun-iter (point) n))))

;;;###autoload
(defun sublimey-toggle-parens ()
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
    (cond ((looking-at-p ")") (sublimey--format-region "[%s]" beg end 1 -1))
          ((looking-at-p "]") (sublimey--format-region "(%s)" beg end 1 -1)))
    (goto-char old-point)))


(provide 'sublimey)

;;; sublimey.el ends here
