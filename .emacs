;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(add-to-list 'load-path (expand-file-name "local" user-emacs-directory))

;; This is only needed once, near the top of the file
(eval-when-compile
  (require 'use-package))

(use-package expand-region
  :config
  (global-set-key (kbd "C-c r") 'er/expand-region))

(use-package ivy
  :init
  (setq ivy-use-virtual-buffers t)
  (setq ivy-use-selectable-prompt t)
  (setq ivy-display-style nil)
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-re-builders-alist
        '((t . ivy--regex-plus)))
  (setq ivy-extra-directories '("./"))
  :config
  (ivy-mode 1))

(use-package swiper
  :bind (("M-s s" . 'swiper)
         ("M-s r" . 'swiper-backward)
         ("M-s p" . 'swiper-thing-at-point)))

(use-package counsel
  :config
  (setq ivy-initial-inputs-alist nil)
  (counsel-mode 1)
  :bind (("C-c k" . 'counsel-ag)
         ("C-c g" . 'counsel-git)
         ("C-c f" . 'counsel-fzf)
         ("C-c j" . 'counsel-git-grep)
         ("C-c p" . 'counsel-file-jump)))

(use-package avy
  :init
  ;; Dvorak home row keys only
  (setq avy-keys '(?a ?o ?e ?u ?h ?t ?n ?s))
  :bind (("M-g M-g" . 'avy-goto-char)
         ("M-g g" . 'avy-goto-subword-1))
  :config
  (face-spec-set 'avy-lead-face '((t (:foreground "white" :background "#e52b50"))))
  (face-spec-set 'avy-lead-face-0 '((t (:foreground "white" :background "#2e36b3")))))

(use-package which-key
  :init
  (setq which-key-add-column-padding 2)
  (setq which-key-separator "-")
  :config
  (which-key-mode))

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
  (setq python-shell-interpreter "python3")
  (setq python-indent-offset 4)
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode))

(use-package markdown-mode
  :mode ("\\.md\\'" "\\.markdown\\'"))

(use-package anaconda-mode
  :hook (python-mode))

(use-package ibuffer
  :bind (:map ibuffer-mode-map
              ("M-o")
              ("M-g")))

(use-package isearch
  :bind (:map isearch-mode-map
              ("C-f" . isearch-yank-char)
              ("C-b" . isearch-del-char)
              ("M-f" . isearch-yank-word-or-char)
              ("C-_" . isearch-delete-char)
              ("C-e" . isearch-yank-line)))

(use-package diff-mode
  :bind (:map diff-mode-map
              ("M-o")))

(use-package text-mode
  :preface (provide 'text-mode)
  :bind (:map text-mode-map
              ("C-a" . 'taemin-back-to-indentation-or-beginning-of-line)))

(use-package prog-mode
  :bind (:map prog-mode-map
              ("C-a" . 'taemin-back-to-indentation-or-beginning-of-line)))

(use-package haskell-mode
  :config
  (add-hook 'haskell-mode-hook
            (lambda () (interactive-haskell-mode)))
  (add-hook 'haskell-interactive-mode-hook
            (lambda () (subword-mode)))
  ;; Prompt build targets on starting the REPL.
  (advice-add 'haskell-session-target
              :around
              #'taemin-advice-haskell-load-prompt))

;; C/C++
(add-hook 'c-mode-hook
	  (lambda () (c-set-style "BSD")))
(add-hook 'c++-mode-hook
	  (lambda () (c-set-style "BSD")))

;; Lisp
(use-package rainbow-delimiters
  :hook ((emacs-lisp-mode . rainbow-delimiters-mode)
         (scheme-mode . rainbow-delimiters-mode)
         (clojure-mode . rainbow-delimiters-mode))
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

(use-package paredit
  :hook ((emacs-lisp-mode . paredit-mode)
         (scheme-mode . paredit-mode)
         (clojure-mode . paredit-mode))
  :bind (:map paredit-mode-map
              ("M-s")
              ("C-M-s" . 'paredit-splice-sexp)
              ("M-r")
              ("C-M-r" . 'paredit-raise-sexp)))

(use-package adjust-parens
  :hook ((emacs-lisp-mode . adjust-parens-mode)
         (scheme-mode . adjust-parens-mode)
         (clojure-mode . adjust-parens-mode)))

(use-package magit
  :bind (("C-x g" . 'magit-status)))

(use-package ag
  :init
  (setq ag-reuse-buffers t))

(use-package flyspell
  :if (or (executable-find "ispell")
          (executable-find "aspell")
          (executable-find "hunspell"))
  :hook ((text-mode . flyspell-mode)))

(use-package goto-last-change
  :bind (("M-g c" . 'goto-last-change)
         ("M-g SPC" . 'goto-last-change-with-auto-marks)))

(use-package undo-fu
  :init
  (setq undo-fu-ignore-keyboard-quit t)
  (defalias 'redo 'undo-fu-only-redo)
  :bind (("C-_" . 'undo-fu-only-undo)
         ("C-/" . 'undo-fu-only-undo)
         ("M-r" . 'undo-fu-only-redo)))

(require 'taemin)
(taemin-select-window-after-compilation t)
(taemin-select-window-after-man t)
(global-set-key (kbd "M-f") 'taemin-forward-word)
(global-set-key (kbd "M-b") 'taemin-backward-word)
(global-set-key (kbd "M-d") 'taemin-kill-word)
(global-set-key (kbd "<M-DEL>") 'taemin-backward-kill-word)
(global-set-key (kbd "C-M-h") 'taemin-mark-defun)
(global-set-key (kbd "M-#") 'taemin-mark-line)
(global-set-key (kbd "<f5>") 'taemin-makefile-compile)
(global-set-key (kbd "<S-f5>") 'taemin-project-compile)
(global-set-key (kbd "C-x C-o") 'taemin-delete-blank-lines)
(global-set-key (kbd "M-k") 'taemin-backward-kill-line)
(global-set-key (kbd "M-h") 'taemin-mark-paragraph)

(require 'untitled-note)
(global-set-key (kbd "C-c n") 'untitled-note-new-note)

(require 'sensible-defaults)
(sensible-defaults/increase-gc-threshold)
(sensible-defaults/delete-trailing-whitespace)
(sensible-defaults/treat-camelcase-as-separate-words)
(sensible-defaults/automatically-follow-symlinks)
(sensible-defaults/make-scripts-executable)
(sensible-defaults/single-space-after-periods)
(sensible-defaults/offer-to-create-parent-directories-on-save)
(sensible-defaults/apply-changes-to-highlighted-region)
(sensible-defaults/overwrite-selected-text)
(sensible-defaults/ensure-that-files-end-with-newline)
(sensible-defaults/quiet-startup)
(sensible-defaults/make-dired-file-sizes-human-readable)
(sensible-defaults/shorten-yes-or-no)
(sensible-defaults/always-highlight-code)
(sensible-defaults/refresh-buffers-when-files-change)
(sensible-defaults/show-matching-parens)
(sensible-defaults/flash-screen-instead-of-ringing-bell)
(sensible-defaults/set-default-line-length-to 80)
(sensible-defaults/open-clicked-files-in-same-frame-on-mac)
(sensible-defaults/yank-to-point-on-mouse-click)

(require 'windmove)
(windmove-default-keybindings)

(defconst emacs-working-directory
  (file-name-as-directory(getenv "PWD"))
  "initial working directory of the emacs process")
(defalias 'elisp-repl 'ielm)
(defalias 'remove-file 'delete-file)
(defalias 'move-file 'rename-file)
(setq-default indent-tabs-mode nil)
;; Prevent shell commands from being echoed in zsh.
(setq explicit-zsh-args '("-o" "no_zle" "-i"))
(setq frame-background-mode 'dark)
(setq help-window-select t)
(setq mark-ring-max 8)
(setq global-mark-ring-max 8)
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(desktop-save-mode 1)
(setq desktop-path `(,emacs-working-directory))
(add-to-list 'desktop-globals-to-save 'compile-command)
(setq isearch-allow-scroll t)
(setq compilation-ask-about-save nil)
(setq compilation-scroll-output 'first-error)
(setq history-length 32)
(when (string-equal system-type "darwin")
  ;; macOS's ls doesn't support --dired option.
  (setq dired-use-ls-dired nil)
  ;; man command completion is too slow and inaccurate.
  (fset 'man 'taemin-man-no-completion))
(setq recentf-save-file
      (expand-file-name ".recentf" emacs-working-directory))
(setq make-backup-files nil)

;; global key bindings
;; unbind old style of keyboard macro bindings. use <f3> and <f4> instead.
(global-unset-key (kbd "C-x e"))
(global-unset-key (kbd "C-x ("))
(global-unset-key (kbd "C-x )"))
(global-set-key [(f6)] 'shell)
(global-set-key (kbd "M-g l") 'goto-line)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "M-%") 'query-replace-regexp)
(global-set-key (kbd "C-x /") 'delete-other-windows)
(global-set-key (kbd "C-x -") 'split-window-below)
(global-set-key (kbd "C-x |") 'split-window-right)
(global-set-key (kbd "C-x w") 'delete-window)
(global-set-key (kbd "M-a") 'backward-paragraph)
(global-set-key (kbd "M-e") 'forward-paragraph)
(global-set-key (kbd "M-{") 'backward-sentence)
(global-set-key (kbd "M-}") 'forward-sentence)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (undo-fu goto-last-change expand-region markdown-mode haskell-mode emamux anaconda-mode which-key cider adjust-parens avy flx rainbow-delimiters geiser paredit company pcre2el glsl-mode magit smex use-package json-mode js2-mode csharp-mode counsel ag vue-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
