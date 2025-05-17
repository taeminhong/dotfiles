;;; blank.el --- Fill in the blank worksheet maker

;;; Code:

(require 'dash)
(require 'untitled-note)

(defvar blank-blanks)

(defun blank--backward-word (n pred)
  "Move backward until encountering the beginning of a word for which
PRED returns non-nil."
  (let ((ok (backward-word n)))
    (while (and ok (not (funcall pred
                                 (point)
                                 (save-excursion (forward-word 1) (point)))))
      (setq ok (backward-word 1)))
    ok))

(defun blank--placeholder-for-word (s)
  (apply 'string
         (mapcar (lambda (c) (if (= c ?') c ?_))
                 s)))

(defun blank--make-blank (start end)
  (if (<= end start)
      (error "Empty blank not allowed")
    `(,start ,end ,(buffer-substring-no-properties start end))))

(defun blank--start (blank)
  (car blank))

(defun blank--end (blank)
  (cadr blank))

(defun blank--string (blank)
  (caddr blank))

(defun blank--buffer-string (blank)
  (buffer-substring-no-properties (blank--start blank) (blank--end blank)))

(defun blank--placeholder (blank)
  (blank--placeholder-for-word (blank--string blank)))

(defun blank--random-range (beg end)
  "Return a pseudo-random number in interval [BEG, END).
BEG and END should be positive."
  (+ beg (random (- end beg))))

(defun blank-remake-worksheet-in-place ()
  "Revert the current buffer and hide random words."
  (interactive)
  (revert-buffer t t t)
  (blank-hide-random-words))

(defun blank-hide-random-words ()
  "Hide random words."
  (save-excursion
    (setq-local blank-blanks nil)
    (goto-char (point-max))
    (while (blank--backward-word (blank--random-range 3 6)
                                 (lambda (start end)
                                   (> (- end start)
                                      1)))
      (let ((word-start (point)))
        (forward-word 1)
        (let ((blank (blank--make-blank (1+ word-start) (point))))
          (goto-char (blank--start blank))
          (kill-region (blank--start blank) (blank--end blank))
          (insert (blank--placeholder blank))
          (push blank blank-blanks)
          (goto-char word-start))))))

(defun blank-check-worksheet ()
  "Check the answer user wrote."
  (interactive)
  (select-window (diff-buffer-with-file (current-buffer))))

(defun blank-next-blank ()
  "Move point to the next blank.
If there is no blanks after point, move point to the first blank."
  (interactive)
  (blank--next-blank))

(defun blank-previous-blank ()
  "Move point to the previous blank.
If there is no blanks before point, move point to the last blank."
  (interactive)
  (blank--next-blank t))

(defun blank-next-incomplete-blank ()
  "Move point to the next incomplete blank."
  (interactive)
  (blank--next-blank nil t))

(defun blank-previous-incomplete-blank ()
  "Move point to the previous incomplete blank."
  (interactive)
  (blank--next-blank t t))

(defun blank--incomplete-blank-p (blank)
  "Return t if BLANK is incomplete."
  (string-match-p "_" (blank--buffer-string blank)))

(defun blank--next-blank (&optional backward ignore-complete-blank)
  "Move point to the next blank.
If there is no blanks after point, move point to the first blank.
if BACKWARD is not nil, operation will be performed in the inverse direction.
This will ignore complete blanks if IGNORE-COMPLETE-BLANK is not nil."
  (let* ((blanks (if backward
                     (reverse blank-blanks)
                   blank-blanks))
         (good-p (if backward
                     (lambda (blank) (< (blank--start blank) (point)))
                   (lambda (blank) (> (blank--start blank) (point)))))
         (index (or (-find-index good-p blanks)
                    (length blanks)))
         (candidates (append (-drop index blanks)
                             (-take index blanks)))
         (next-blank (if ignore-complete-blank
                         (-first #'blank--incomplete-blank-p candidates)
                       (car candidates))))
    (if next-blank
        (goto-char (car next-blank))
      (user-error "No blanks"))))

(defun blank-make-worksheet ()
  "Make a new fill-in-the-blank worksheet from the contents of the current buffer."
  (interactive)
  (let ((org-buf (current-buffer))
        (new-buf (untitled-note-generate-new-buffer "*Fill In The Blank*"))
        (start (if (use-region-p) (region-beginning) (point-min)))
        (end (if (use-region-p) (region-end) (point-max)))
        (org-point (point)))
    (with-current-buffer new-buf
      (let ((inhibit-message t))
        (insert-buffer-substring org-buf start end)
        (goto-char (1+ (- org-point start)))
        (save-buffer)
        (blank-mode 1)
        (overwrite-mode 1)
        (blank-hide-random-words)))
    (switch-to-buffer new-buf)))

(defun blank--find-blank-at (pos &optional lower-inclusive upper-inclusive)
  (-first (lambda (blank)
            (and (if lower-inclusive
                     (<= (blank--start blank) pos)
                   (< (blank--start blank) pos))
                 (if upper-inclusive
                     (<= pos (blank--end blank))
                   (< pos (blank--end blank)))))
          blank-blanks))

(defun blank-delete-forward-char ()
  (interactive)
  (let ((blank (blank--find-blank-at (point) t nil)))
    (when blank
      (let ((offset (- (point) (blank--start blank))))
        (save-excursion
          (atomic-change-group
            (delete-char 1)
            (insert (substring (blank--placeholder blank) offset (+ offset 1)))))))))

(defun blank-delete-backward-char ()
  (interactive)
  (let ((blank (blank--find-blank-at (point) nil t)))
    (if blank
        (let ((offset (- (point) (blank--start blank) 1)))
          (atomic-change-group
            (delete-char -1)
            (insert (substring (blank--placeholder blank) offset (+ offset 1)))
            (goto-char (1- (point)))))
      (backward-char))))

(defun blank-backward-kill-word ()
  (interactive)
  (let ((blank (blank--find-blank-at (point) nil t)))
    (if blank
        (let ((n (- (point) (blank--start blank))))
          (atomic-change-group
            (kill-region (blank--start blank) (point))
            (insert (substring (blank--placeholder blank) 0 n))
            (goto-char (blank--start blank))))
      (backward-word))))

(defun blank-forward-kill-word ()
  (interactive)
  (let ((blank (blank--find-blank-at (point) t nil)))
    (when blank
      (let ((offset (- (point) (blank--start blank))))
        (save-excursion
          (atomic-change-group
            (kill-region (point) (blank--end blank))
            (insert (substring (blank--placeholder blank) offset))))))))

(defun blank-fill-correct-answer ()
  (interactive)
  (let ((blank (blank--find-blank-at (point) t t)))
    (when blank
      (atomic-change-group
        (goto-char (blank--start blank))
        (kill-region (blank--start blank) (blank--end blank))
        (insert (blank--string blank))
        (goto-char (blank--end blank))))))

(defvar blank-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") 'blank-next-incomplete-blank)
    (define-key map (kbd "<backtab>") 'blank-previous-incomplete-blank)
    (define-key map (kbd ">") 'blank-next-blank)
    (define-key map (kbd "<") 'blank-previous-blank)
    (define-key map (kbd "C-c C-c") 'blank-check-worksheet)
    (define-key map (kbd "C-c C-r") 'blank-remake-worksheet-in-place)
    (define-key map (kbd "DEL") 'blank-delete-backward-char)
    (define-key map (kbd "C-d") 'blank-delete-forward-char)
    (define-key map (kbd "M-DEL") 'blank-backward-kill-word)
    (define-key map (kbd "M-d") 'blank-forward-kill-word)
    (define-key map (kbd "RET") 'blank-fill-correct-answer)
    map))

(define-minor-mode blank-mode
  "Fill in the blank mode"
  :init-value nil
  :lighter " Blank"
  :keymap blank-mode-map)

(provide 'blank)
;;; blank.el ends here
