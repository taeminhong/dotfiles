(require 'misc)

(defun my-forward-to-word (n)
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

(defun my-kill-word (n)
  (interactive "^p")
  (while (< 0 n)
    (if (looking-at-p "\\w")
        (kill-word 1)
      (let ((beg (point))
            (end (save-excursion (forward-to-word 1) (point))))
        (kill-region beg end)))
    (setq n (1- n))))

(defun my-backward-kill-word (n)
  (interactive "^p")
  (while (< 0 n)
    (if (looking-back "\\w" (- (point) 1))
        (backward-kill-word 1)
      (let ((beg (point))
            (end (save-excursion (backward-to-word 1) (point))))
        (kill-region beg end)))
    (setq n (1- n))))

(defun last-sexp-in-list-p ()
  (condition-case nil
      (save-excursion
        (forward-sexp) nil)
    (scan-error nil t)))

(defun no-more-sexp-p ()
  (let ((prev-point (point)))
    (condition-case err
      (save-excursion
        (forward-sexp)
        (backward-sexp)
        (forward-sexp)
        (<= (point) prev-point))
      (scan-error nil t))))

(defun my-next-sexp-iter (start-point n)
  (cond ((<= n 0) (point))
        ((last-sexp-in-list-p)
         (when (= (point) start-point)
           ;; raise a scan error
           (forward-sexp)))
        ((no-more-sexp-p)
         (forward-sexp))
        (t
         (forward-sexp)
         (backward-sexp)
         (if (> (point) start-point)
             (my-next-sexp-iter (point) (- n 1))
           (progn (forward-sexp)
                  (my-next-sexp-iter start-point n))))))

(defun my-next-sexp (n)
  (interactive "^p")
  (cond ((< n 0) (backward-sexp (- n)))
        (t (my-next-sexp-iter (point) n))))

(defun make-range (beg end)
  (cons beg end))

(defun beginning (range)
  (car range))

(defun end (range)
  (cdr range))

(defun nearby-defun (pos)
  (let (end)
    (save-excursion
      (goto-char pos)
      (end-of-defun)
      (setq end (point))
      (beginning-of-defun)
      (make-range (point) end))))

(defun my-next-defun (n)
  (interactive "^p")
  (cond ((< n 0)
         (beginning-of-defun (- n)))
        ((> n 0)
         (or (not (eq this-command 'my-next-defun))
             (eq last-command 'my-next-defun)
             (and transient-mark-mode mark-active)
             (push-mark))
         (my-next-defun-iter (point) n))))

(defun my-next-defun-iter (start-point n)
  (cl-assert (<= start-point (point)))
  (cond ((<= n 0)
         (point))
        ((= start-point (point-max))
         (signal 'end-of-buffer start-point))
        (t
         (let ((nearby (nearby-defun (point))))
           (cond ((> (beginning nearby) start-point)
                  (goto-char (beginning nearby))
                  (my-next-defun-iter (beginning nearby) (- n 1)))
                 ((> (end nearby) (point))
                  (goto-char (end nearby))
                  (my-next-defun-iter start-point n))
                 (t
                  (goto-char (point-max))
                  (my-next-defun-iter (point-max) (- n 1))))))))

(defun my-electric-pair-conservative-inhibit (char)
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

(provide 'taemin)
