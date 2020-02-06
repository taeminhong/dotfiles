;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(let ((default-directory "~/.emacs.d/local"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;; This is only needed once, near the top of the file
(eval-when-compile
  (require 'use-package))

(use-package ivy
  :init
  (setq ivy-use-virtual-buffers nil)
  (setq ivy-use-selectable-prompt t)
  (setq ivy-display-style nil)
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-re-builders-alist
        '((t . ivy--regex-plus)))
  :config
  (ivy-mode 1))

(use-package counsel
  :config
  (setq ivy-initial-inputs-alist nil)
  (counsel-mode 1)
  :bind (("C-c k" . 'counsel-ag)
         ("C-c g" . 'counsel-git)
         ("C-c f" . 'counsel-fzf)
         ("C-c j" . 'counsel-git-grep)))

(use-package avy
  :bind (("M-g l" . 'avy-goto-line)
         ("M-g c" . 'avy-goto-subword-1)))

(use-package js2-mode
  :init
  (setq js2-missing-semi-one-line-override t)
  (setq js2-strict-missing-semi-warning nil)
  (setq js2-basic-offset 4)
  :mode "\\.js\\'")

(use-package json-mode
  :init
  ;; use 4 spaces in js, but 2 spaces in json by default
  (setq json-reformat:indent-width 2)
  (add-hook 'json-mode-hook
            (lambda () (setq-local js-indent-level 2))))

;; The package is "python" but the mode is "python-mode":
(use-package python
  :init
  (setq python-indent-offset 4)
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode))

;; C/C++
(add-hook 'c-mode-hook
	  (lambda () (c-set-style "BSD")))
(add-hook 'c++-mode-hook
	  (lambda () (c-set-style "BSD")))

;; Lisp
(use-package rainbow-delimiters
  :init
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'scheme-mode-hook #'rainbow-delimiters-mode)
  :config
  ;; Don't use the :custom-face keyword.
  ;; It's going to mess up the custom-set-faces list at bottommost.
  (face-spec-set 'rainbow-delimiters-depth-1-face '((t (:foreground "dark orange"))))
  (face-spec-set 'rainbow-delimiters-depth-2-face '((t (:foreground "magenta"))))
  (face-spec-set 'rainbow-delimiters-depth-3-face '((t (:foreground "chartreuse"))))
  (face-spec-set 'rainbow-delimiters-depth-4-face '((t (:foreground "deep sky blue"))))
  (face-spec-set 'rainbow-delimiters-depth-5-face '((t (:foreground "yellow"))))
  (face-spec-set 'rainbow-delimiters-depth-6-face '((t (:foreground "orchid"))))
  (face-spec-set 'rainbow-delimiters-depth-7-face '((t (:foreground "spring green"))))
  (face-spec-set 'rainbow-delimiters-depth-8-face '((t (:foreground "sienna1")))))

(use-package paredit-mode
  :hook (emacs-lisp-mode scheme-mode))

;; Misc
(use-package taemin
  :bind (("M-d" . taemin-kill-word)
         ("<M-DEL>" . taemin-backward-kill-word)
         ("C-M-p" . beginning-of-defun)
         ("C-M-n" . taemin-next-defun)
         ("C-M-h" . taemin-mark-defun)
         ("M-#" . taemin-mark-line)))

(require 'move-lines)
(move-lines-binding)
(require 'sensible-defaults)
(sensible-defaults/use-all-settings)
(require 'windmove)
(windmove-default-keybindings)
(setq-default indent-tabs-mode nil)
(setq frame-background-mode 'dark)
(setq help-window-select t)
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(desktop-save-mode 1)
(setq desktop-path '("."))
;; Suppress ls-dired warning in OSX
(setq dired-use-ls-dired
      (not (string-equal system-type "darwin")))

;; global key bindings
(global-set-key [(f5)] 'compile)
(global-set-key [(f6)] 'shell)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "M-%") 'query-replace-regexp)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (avy flx rainbow-delimiters geiser paredit company pcre2el glsl-mode magit smex use-package json-mode js2-mode csharp-mode counsel ag vue-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'magit-clean 'disabled nil)
