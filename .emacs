;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'misc)

(defun my-forward-word-begin (n)
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

(defun my-backward-word-end (n)
  (interactive "^p")
  (let (word-begin
        word-end
        subword-begin-p)
    (while (< 0 n)
      (save-excursion
        (backward-to-word 1)
        (setq word-end (point)))
      (save-excursion
        (backward-word 1)
        (setq word-begin (point))
        (setq subword-begin-p (looking-back "\\w" (1- (point)))))
      (if (and (< word-end word-begin) subword-begin-p)
          (goto-char word-begin)
        (goto-char word-end))
      (setq n (1- n)))))

(defalias 'my-backward-word-begin 'backward-word)
(defalias 'my-forward-word-end 'forward-word)

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

;; Suppress ls-dired warning in OSX
(setq dired-use-ls-dired
      (not (string-equal system-type "darwin")))

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; indentation
(setq-default indent-tabs-mode nil)
(setq python-indent-offset 4)
;; use 4 spaces in js, but 2 spaces in json by default
(setq js2-basic-offset 4)
(setq json-reformat:indent-width 2)
(add-hook 'json-mode-hook
          (lambda () (setq-local js-indent-level 2)))

;; C/C++
(add-hook 'c-mode-hook
	  (lambda () (c-set-style "BSD")))
(add-hook 'c++-mode-hook
	  (lambda () (c-set-style "BSD")))

;; Ivy/Counsel/Swiper
(use-package ivy
  :init
  (setq ivy-use-virtual-buffers nil)
  (setq ivy-use-selectable-prompt t)
  (setq ivy-display-style nil)
  ;; Better fuzzy match support in Ivy
  ;; see https://oremacs.com/2016/01/06/ivy-flx/
  (setq ivy-re-builders-alist
        '((t . ivy--regex-fuzzy)))
  (setq ivy-initial-inputs-alist nil)
  :config
  (ivy-mode 1))


(use-package counsel
  :config
  (counsel-mode 1)
  :bind (("C-c k" . 'counsel-ag)
         ("C-c g" . 'counsel-git)
         ("C-c f" . 'counsel-fzf)
         ("C-c j" . 'counsel-git-grep)))

;; key bindings
(global-set-key [(f5)] 'compile)
(global-set-key [(f6)] 'shell)
(global-set-key "\M-g" 'goto-line)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-d") 'my-kill-word)
(global-set-key (kbd "<M-DEL>") 'my-backward-kill-word)
(global-set-key (kbd "<C-right>") 'my-forward-word-begin)
(global-set-key (kbd "<C-left>") 'my-backward-word-begin)
(global-set-key (kbd "<M-left>") 'my-backward-word-end)
(global-set-key (kbd "<M-right>") 'my-forward-word-end)
(windmove-default-keybindings)

;; Packages
;; This code came from https://www.emacswiki.org/emacs/ELPA
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

;; Smooth scroll
(setq scroll-step           1
      scroll-conservatively 10000)

;; Sensible Emacs
(load-file "~/.emacs.d/sensible-defaults.el")
(sensible-defaults/use-all-settings)

;; Look and feel
(setq frame-background-mode 'dark)

;; create temporary buffer
(defun generate-buffer ()
  (interactive)
  (switch-to-buffer (make-temp-name "scratch")))

;; Scheme
(setq scheme-program-name "guile")

(load-file "~/.emacs.d/move-lines.el")
(move-lines-binding)

;; Misc
(setq help-window-select t)
(desktop-save-mode 1)
(setq desktop-path '("."))
(electric-pair-mode 1)
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
(setq electric-pair-inhibit-predicate 'my-electric-pair-conservative-inhibit)
(setq electric-pair-skip-whitespace nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(js2-missing-semi-one-line-override t)
 '(js2-strict-missing-semi-warning nil)
 '(package-selected-packages
   (quote
    (company pcre2el glsl-mode magit flx smex use-package json-mode js2-mode csharp-mode counsel swiper projectile ag vue-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'magit-clean 'disabled nil)
