;;; blank.el --- Fill in the blank worksheet maker

;;; Code:

(require 'cl-lib)
(require 'untitled-note)

(defun blank--backward-non-one-letter-word ()
  "Move backward until encountering the beginning of non-one-letter word."
  (let ((ok (backward-word 1)))
    (while (and ok
                (not (looking-at-p "\\w\\w")))
      (setq ok (backward-word 1)))
    ok))

(defun blank--placeholder (s)
  (let ((head (substring s 0 1))
        (tail (substring s 1)))
    (concat head
            (apply 'string
                   (mapcar (lambda (c) (if (= c ?') c ?_))
                           tail)))))

(defun blank-hide-word ()
  "Hide a word.
ie) hello -> h____"
  (let* ((start (point))
         (end (save-excursion (forward-word 1) (point)))
         (placeholder (blank--placeholder (buffer-substring start end))))
    (kill-word 1)
    (insert placeholder)
    (goto-char start)))

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
    (while (and (backward-word (blank--random-range 2 5))
                (blank--backward-non-one-letter-word))
      (let ((start (point)))
        (forward-word 1)
        (let ((word (buffer-substring start (point))))
          (kill-region start (point))
          (insert (blank--placeholder word))
          (push (1+ start) blank-blanks)
          (goto-char start))))))

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

(defun blank-next-non-fully-filled-blank ()
  "Move point to the next non fully filled blank."
  (interactive)
  (blank--next-blank nil t))

(defun blank-previous-non-fully-filled-blank ()
  "Move point to the previous non fully filled blank."
  (interactive)
  (blank--next-blank t t))

(defun blank-nth (n list)
  "Return the Nth element of List.
N counts from zero and can be a negative number.
-1 means the last, -2 means the next to the last."
  (let ((l (length list)))
    (if (= l 0)
        nil
      (nth (% (+ (% n l) l) l) list))))

(defun blank--non-fully-filled-blank-p (pos)
  "Return t if POS points to a non fully filled blank."
  (save-excursion
    (goto-char pos)
    (looking-at-p "[^[:space:]_]*_")))

(defun blank--next-blank (&optional backward ignore-fully-filled-blank)
  "Move point to the next blank.
If there is no blanks after point, move point to the first blank.
if BACKWARD is not nil, operation will be performed in the inverse direction.
This will ignore fully filled blanks if IGNORE-FULLY-FILLED-BLANK is not nil."
  (let ((blanks (if ignore-fully-filled-blank
                    (seq-filter 'blank--non-fully-filled-blank-p blank-blanks)
                  blank-blanks))
        (cmp (if backward '< '>))
        (idx (if backward -1 0)))
    (if blanks
        (progn
          (goto-char (or (blank-nth idx (seq-filter (lambda (x) (funcall cmp x (point)))
                                                    blanks))
                         (blank-nth idx blanks))))
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

(defvar blank-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") 'blank-next-non-fully-filled-blank)
    (define-key map (kbd "<backtab>") 'blank-previous-non-fully-filled-blank)
    (define-key map (kbd ">") 'blank-next-blank)
    (define-key map (kbd "<") 'blank-previous-blank)
    (define-key map (kbd "C-c C-c") 'blank-check-worksheet)
    (define-key map (kbd "C-c C-r") 'blank-remake-worksheet-in-place)
    map))

(define-minor-mode blank-mode
  "Fill in the blank mode"
  nil
  " Blank"
  blank-mode-map)

(provide 'blank)
;;; blank.el ends here
