(require 'misc)

(defun taemin-forward-to-word (n)
  (interactive "^p")
  (let (word-begin
        word-end
        subword-end-p)
    (while (< 0 n)
      (save-excursion
        (forward-to-word 1)
        (setq word-begin (point)))
      (save-excursion
        (forward-word 1)
        (setq word-end (point))
        (setq subword-end-p (looking-at-p "\\w")))
      (if (or (< word-begin word-end) (not subword-end-p))
          (goto-char word-begin)
        (goto-char word-end))
      (setq n (1- n)))))

(defun taemin-kill-word (n)
  (interactive "^p")
  (while (< 0 n)
    (if (looking-at-p "\\w")
        (kill-word 1)
      (let ((beg (point))
            (end (save-excursion (forward-to-word 1) (point))))
        (kill-region beg end)))
    (setq n (1- n))))

(defun taemin-backward-kill-word (n)
  (interactive "^p")
  (while (< 0 n)
    (if (looking-back "\\w" (- (point) 1))
        (backward-kill-word 1)
      (let ((beg (point))
            (end (save-excursion (backward-to-word 1) (point))))
        (kill-region beg end)))
    (setq n (1- n))))

(defun taemin--last-sexp-in-list-p ()
  (condition-case nil
      (save-excursion
        (forward-sexp) nil)
    (scan-error nil t)))

(defun taemin--no-more-sexp-p ()
  (let ((prev-point (point)))
    (condition-case err
      (save-excursion
        (forward-sexp)
        (backward-sexp)
        (forward-sexp)
        (<= (point) prev-point))
      (scan-error nil t))))

(defun taemin-next-sexp-iter (start-point n)
  (cond ((<= n 0) (point))
        ((taemin--last-sexp-in-list-p)
         (when (= (point) start-point)
           ;; raise a scan error
           (forward-sexp)))
        ((taemin--no-more-sexp-p)
         (forward-sexp))
        (t
         (forward-sexp)
         (backward-sexp)
         (if (> (point) start-point)
             (taemin-next-sexp-iter (point) (- n 1))
           (progn (forward-sexp)
                  (taemin-next-sexp-iter start-point n))))))

(defun taemin-next-sexp (n)
  (interactive "^p")
  (cond ((< n 0) (backward-sexp (- n)))
        (t (taemin-next-sexp-iter (point) n))))

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

(defun taemin-mark-defun (&optional arg)
  "Put mark at beginning of this defun, point at beginning of next defun.
The defun marked is the one that contains point or follows point.
With positive ARG, mark this and that many next defuns; with negative
ARG, change the direction of marking.

If the mark is active, it marks the next or previous defun(s) after
the one(s) already marked."
  (interactive "p")
  (setq arg (or arg 1))
  ;; There is no `taemin-mark-defun-back' function - see
  ;; https://lists.gnu.org/r/bug-gnu-emacs/2016-11/msg00079.html
  ;; for explanation
  (when (eq last-command 'taemin-mark-defun-back)
    (setq arg (- arg)))
  (when (< arg 0)
    (setq this-command 'taemin-mark-defun-back))
  (cond ((or (use-region-p) (taemin--turning-back-mark-defun-p))
         (if (>= arg 0)
             (taemin--end-of-defun-spaces arg)
           (beginning-of-defun-comments (- arg))))
        (t
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
           (taemin--expand-defun-mark arg beg end)))))

(provide 'taemin)
