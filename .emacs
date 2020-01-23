;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(let ((default-directory "~/.emacs.d/local"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

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
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-d") 'my-kill-word)
(global-set-key (kbd "<M-DEL>") 'my-backward-kill-word)
(global-set-key (kbd "M-f") 'my-forward-to-word)
(global-set-key (kbd "M-e") 'forward-word)
(global-set-key (kbd "C-M-f") 'my-next-sexp)
(global-set-key (kbd "C-M-e") 'forward-sexp)
(global-set-key (kbd "C-M-n") 'my-next-defun)
(global-set-key (kbd "C-M-p") 'beginning-of-defun)
(windmove-default-keybindings)

;; Packages
;; This code came from https://www.emacswiki.org/emacs/ELPA
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

;; Smooth scroll
(setq scroll-step           1
      scroll-conservatively 10000)

(require 'taemin)

;; Sensible Emacs
(require 'sensible-defaults)
(sensible-defaults/use-all-settings)

;; Look and feel
(setq frame-background-mode 'dark)

;; create temporary buffer
(defun generate-buffer ()
  (interactive)
  (switch-to-buffer (make-temp-name "scratch")))

;; Scheme
(setq scheme-program-name "racket")

(require 'move-lines)
(move-lines-binding)

;; Misc
(setq help-window-select t)
(desktop-save-mode 1)
(setq desktop-path '("."))
(electric-pair-mode 1)
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
    (geiser paredit company pcre2el glsl-mode magit smex use-package json-mode js2-mode csharp-mode counsel ag vue-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'magit-clean 'disabled nil)
