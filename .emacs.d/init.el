(defconst emacs-working-directory
  (file-name-as-directory (getenv "PWD"))
  "initial working directory of the emacs process")

(defconst is-macos (eq system-type 'darwin))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(add-to-list 'load-path (expand-file-name "local" user-emacs-directory))

(defalias 'elisp-repl 'ielm)
(defalias 'remove-file 'delete-file)
(defalias 'move-file 'rename-file)
(fset 'yes-or-no-p 'y-or-n-p)

;; Prevent shell commands from being echoed in zsh.
(defvar explicit-zsh-args '("-o" "no_zle" "-i"))

(defun other-window-backward ()
  (interactive)
  (other-window -1 nil t))

(defun split-select-window-below ()
  (interactive)
  (select-window (split-window-below)))

(defun split-select-window-right ()
  (interactive)
  (select-window (split-window-right)))

(setq-default indent-tabs-mode nil)
(setq-default fill-column 80)
(setq frame-background-mode 'dark)
(setq help-window-select t)
(setq mark-ring-max 8)
(setq global-mark-ring-max 8)
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq isearch-allow-scroll t)
(setq isearch-regexp-lax-whitespace t)
(setq search-whitespace-regexp ".*?")
(setq history-length 32)
(setq make-backup-files nil)
(setq gc-cons-threshold 20000000)
(setq vc-follow-symlinks t)
(setq sentence-end-double-space nil)
(setq require-final-newline t)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq ring-bell-function 'ignore)
(setq c-default-style '((c-mode . "bsd")
                        (c++-mode . "bsd")
                        (java-mode . "java")
                        (awk-mode . "awk")
                        (other . "gnu")))

(menu-bar-mode -1)
(transient-mark-mode t)
(delete-selection-mode t)
(global-font-lock-mode t)
(global-auto-revert-mode t)
(show-paren-mode t)
(column-number-mode t)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'prog-mode-hook 'subword-mode)
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; unbind old style of keyboard macro bindings. use <f3> and <f4> instead.
(global-unset-key (kbd "C-x e"))
(global-unset-key (kbd "C-x ("))
(global-unset-key (kbd "C-x )"))
(global-unset-key (kbd "C-x o"))
(global-set-key (kbd "C-o") 'split-line)
(global-set-key (kbd "C-M-o") 'open-line)
(global-set-key (kbd "M-g l") 'goto-line)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "M-%") 'query-replace-regexp)
(global-set-key (kbd "C-x /") 'delete-other-windows)
(global-set-key (kbd "C-x -") 'split-select-window-below)
(global-set-key (kbd "C-x |") 'split-select-window-right)
(global-set-key (kbd "C-x w") 'write-file)
(global-set-key (kbd "C-x s") 'save-buffer)
(global-set-key (kbd "C-x S") 'save-some-buffers)
(global-set-key (kbd "C-x o b") 'switch-to-buffer-other-window)
(global-set-key (kbd "C-x o f") 'find-file-other-window)
(global-set-key (kbd "C-x o i") 'display-buffer)
(global-set-key (kbd "C-x o .") 'xref-find-definitions-other-window)
(global-set-key (kbd "M-a") 'backward-paragraph)
(global-set-key (kbd "M-e") 'forward-paragraph)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-x DEL") 'kill-whole-line)
(global-set-key (kbd "M-{") 'backward-sentence)
(global-set-key (kbd "M-}") 'forward-sentence)
(global-set-key (kbd "M-u") 'upcase-dwim)
(global-set-key (kbd "M-l") 'downcase-dwim)
(global-set-key (kbd "M-c") 'capitalize-dwim)
(global-set-key (kbd "M-RET") 'comment-indent-new-line)
(global-set-key (kbd "M-'") 'dabbrev-expand)

(when (display-graphic-p)
  ;; <up> and <down> are usually interpreted as "M-O A" and "M-O B" on console.
  ;; So if we bind "M-O", those keys might not work. Let's bind "M-O" only when
  ;; running on GUI.
  (global-set-key (kbd "M-O") 'other-window-backward))

(require 'desktop)
(setq desktop-path `(,emacs-working-directory))
(add-to-list 'desktop-globals-to-save 'compile-command)
(desktop-save-mode 1)

(require 'dired)
;; macOS's ls doesn't support --dired option.
(setq dired-use-ls-dired (not is-macos))
(setq dired-listing-switches "-alh")
(global-set-key (kbd "C-x o d") 'dired-other-window)
(add-hook 'dired-mode-hook #'dired-hide-details-mode)
(add-hook 'dired-mode-hook #'auto-revert-mode)

(require 'compile)
(setq compilation-scroll-output 'first-error)
(setq compilation-ask-about-save nil)

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (ansi-color-apply-on-region compilation-filter-start (point-max)))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(require 'recentf)
(setq recentf-save-file
      (expand-file-name ".recentf" emacs-working-directory))

(require 'taemin)
(taemin-select-window-after-compilation t)
(taemin-select-window-after-man t)
(when is-macos
  ;; man command completion is too slow and inaccurate.
  (fset 'man 'taemin-man-no-completion)
  (unless native-comp-driver-options
    (customize-set-variable 'native-comp-driver-options '("-Wl,-w"))))
(add-hook 'after-init-hook 'taemin-show-init-time)
(add-hook 'before-save-hook 'taemin-create-buffer-file-parent-directories)
(global-set-key (kbd "<f5>") 'taemin-makefile-compile)
(global-set-key (kbd "<S-f5>") 'taemin-project-compile)
(global-set-key (kbd "<f6>") 'taemin-terminal-other-window-reuse)
(global-set-key (kbd "<S-f6>") 'taemin-terminal-other-window)
(global-set-key (kbd "M-s o") 'taemin-occur)

(require 'untitled-note)
(global-set-key (kbd "C-c n") 'untitled-note-new-note)
(define-key untitled-note-mode-map (kbd "C-c C-k") 'taemin-kill-this-buffer-no-prompt)

(require 'blank)
(define-key blank-mode-map (kbd "C-c C-k") 'taemin-kill-this-buffer-no-prompt)

;; This is only needed once, near the top of the file
(eval-when-compile
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
  (require 'use-package))

(use-package tmacs
  :init
  (global-set-key (kbd "C-x o o o") 'other-window)
  (global-set-key (kbd "C-x o o O") 'other-window-backward)
  :bind (("C-x o o t" . tmacs-other-window-tmux-aware)
         ("C-x o o T" . tmacs-other-window-tmux-aware-backward)
         ("C-x o o q" . tmacs-other-window-quiet)
         ("C-x o o Q" . tmacs-other-window-quiet-backward)))

(use-package term
  :bind (:map term-raw-map
              ("M-o")
              ("M-x")))

(use-package view
  :bind (([f8] . view-mode)
         :map view-mode-map
         ;; Delete all keybindings for exiting mode.
         ("c") ("C") ("q") ("Q") ("e") ("E")
         (("G". end-of-buffer))
         (("j" . View-scroll-line-forward))
         (("k" . View-scroll-line-backward))
         (("b" . View-scroll-page-backward))
         (("o" . occur))))

(use-package dictionary
  :init
  (setq dictionary-server "dict.org")
  :bind (("M-s d" . dictionary-search)))

(use-package sublimey
  :init
  (setq sublimey-word-leap-empty-lines t)
  :bind (("M-f" .     sublimey-forward-word)
         ("M-b" .     sublimey-backward-word)
         ("M-d" .     sublimey-kill-word)
         ("<M-DEL>" . sublimey-backward-kill-word)
         ("C-M-h" .   sublimey-mark-defun)
         ("M-#" .     sublimey-mark-line)
         ("C-x C-o" . sublimey-delete-blank-lines)
         ("M-k" .     sublimey-backward-kill-line)
         ("M-e" .     sublimey-forward-paragraph)
         ("M-a" .     sublimey-backward-paragraph)
         ("M-h" .     sublimey-mark-paragraph)
         ("C-x M-d" . sublimey-kill-paragraph)
         ("C-x M-DEL" . sublimey-backward-kill-paragraph)
         :map text-mode-map
         ("C-a" .     sublimey-back-to-indentation-or-beginning-of-line)
         :map prog-mode-map
         ("C-a" .     sublimey-back-to-indentation-or-beginning-of-line)))

(use-package expand-region
  :bind ("C-c r" . er/expand-region))

(use-package ivy
  :ensure t
  :diminish ivy-mode
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

(use-package counsel
  :ensure t
  :demand
  :diminish counsel-mode
  :config
  (setq ivy-initial-inputs-alist nil)
  (counsel-mode 1)
  :bind (("C-x f" . counsel-find-file)
         ("C-c k" . counsel-ag)
         ("C-c g" . counsel-git)
         ("C-c f" . counsel-fzf)
         ("C-c j" . counsel-git-grep)
         ("C-c p" . counsel-file-jump)))

(use-package ivy-xref
  :defer t
  :init
  (setq xref-show-xrefs-function 'ivy-xref-show-xrefs))

(use-package avy
  :ensure t
  :init
  ;; Dvorak home row keys only
  (setq avy-keys '(?a ?o ?e ?u ?h ?t ?n ?s))
  :bind (("M-g M-c" . avy-goto-char-timer)
         ("M-g M-l" . avy-goto-line))
  :config
  (face-spec-set 'avy-lead-face '((t (:foreground "white" :background "#e52b50"))))
  (face-spec-set 'avy-lead-face-0 '((t (:foreground "white" :background "#2e36b3")))))

(use-package hydra
  :ensure t
  :config
  (defhydra hydra-window-resize (global-map "C-x" :foreign-keys warn)
    "window resize"
    ("[" shrink-window)
    ("]" enlarge-window)
    ("{" shrink-window-horizontally)
    ("}" enlarge-window-horizontally)
    ("RET" nil "quit")))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :init
  (setq which-key-add-column-padding 2)
  (setq which-key-separator "-")
  :config
  (which-key-mode))

(use-package sgml-mode
  :init
  (add-hook 'sgml-mode-hook
            (lambda ()
              (kill-local-variable 'paragraph-start)
              (kill-local-variable 'paragraph-separate)))
  :bind (:map html-mode-map
              ("M-o"))
  :config
  (setq sgml-quick-keys 'close))

(use-package js2-mode
  :init
  (setq js2-missing-semi-one-line-override t)
  (setq js2-strict-missing-semi-warning nil)
  (setq js2-basic-offset 4)
  :mode "\\.js\\'")

(use-package json-mode
  :defer t
  :init
  ;; use 4 spaces in js, but 2 spaces in json by default
  (setq json-reformat:indent-width 2)
  (add-hook 'json-mode-hook
            (lambda () (setq-local js-indent-level 2))))

;; The package is "python" but the mode is "python-mode":
(use-package python
  :defer t
  :init
  (setq python-shell-interpreter "python3")
  (setq python-indent-offset 4))

(use-package anaconda-mode
  :hook (python-mode))

(use-package ibuffer
  :bind (:map ibuffer-mode-map
              ("M-o")
              ("M-g")))

(use-package diff-mode
  :bind (:map diff-mode-map
              ("M-o")))

(use-package elisp-mode
  ;; lisp-mode-shared-map is the parent key map of lisp-mode-map,
  ;; emacs-lisp-mode-map, and lisp-interaction-mode-map
  :bind (:map lisp-mode-shared-map
              ("C-x e e" . eval-last-sexp)
              ("C-x e d" . eval-defun)
              ("C-x e r" . eval-region)
              ("C-x e b" . eval-buffer)))

(use-package haskell-mode
  :defer t
  :init
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (add-hook 'haskell-interactive-mode-hook 'subword-mode)
  :config
  ;; Prompt build targets on starting the REPL.
  (advice-add 'haskell-session-target
              :around
              #'taemin-advice-haskell-load-prompt))

(use-package dante
  :hook (haskell-mode . dante-mode))

(use-package rainbow-delimiters
  :ensure t
  :hook ((emacs-lisp-mode . rainbow-delimiters-mode)
         (scheme-mode . rainbow-delimiters-mode)
         (clojure-mode . rainbow-delimiters-mode)
         (haskell-mode . rainbow-delimiters-mode))
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
  :ensure t
  :hook ((emacs-lisp-mode . paredit-mode)
         (scheme-mode . paredit-mode)
         (clojure-mode . paredit-mode))
  :bind (:map paredit-mode-map
              ("M-s")
              ("C-M-s" . paredit-splice-sexp)
              ("M-r")
              ("C-M-r" . paredit-raise-sexp)))

(use-package adjust-parens
  :ensure t
  :hook ((emacs-lisp-mode . adjust-parens-mode)
         (scheme-mode . adjust-parens-mode)
         (clojure-mode . adjust-parens-mode)))

(use-package magit
  :bind (("C-x g" . magit-status)))

(use-package ag
  :defer t
  :init
  (setq ag-reuse-buffers t))

(use-package flyspell
  :if (or (executable-find "ispell")
          (executable-find "aspell")
          (executable-find "hunspell"))
  :diminish flyspell-mode
  :hook ((text-mode . flyspell-mode)))

(use-package goto-last-change
  :bind (("M-g c")
         ("M-g c h" . goto-last-change)
         ("M-g SPC" . goto-last-change-with-auto-marks)))

(use-package undo-fu
  :ensure t
  :init
  (setq undo-fu-ignore-keyboard-quit t)
  (defalias 'redo 'undo-fu-only-redo)
  :bind (("C-_" . undo-fu-only-undo)
         ("C-/" . undo-fu-only-undo)
         ("M-/" . undo-fu-only-redo)))

(use-package web-mode
  :mode ("\\.[jt]sx\\'"))

(use-package tide
  :after (typescript-mode web-mode)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (web-mode . tide-setup)
         (web-mode . tide-hl-identifier-mode)))

(use-package w3m
  :bind ((:map w3m-mode-map
               ("C-e" . move-end-of-line)
               ("C-a" . move-beginning-of-line)
               ("<" . w3m-tab-previous-buffer)
               (">" . w3m-tab-next-buffer)))
  :config
  (face-spec-set 'w3m-anchor '((t (:foreground "blue" :bold nil :underline nil))))
  (face-spec-set 'w3m-arrived-anchor '((t (:foreground "cyan" :bold nil :underline nil))))
  (face-spec-set 'w3m-image '((t (:foreground "green"))))
  (face-spec-set 'w3m-image-anchor '((t (:foreground "green" :background "black" :underline nil))))
  (face-spec-set 'w3m-header-line-content '((t (:foreground "white" :background "black"))))
  (face-spec-set 'w3m-header-line-title '((t (:foreground "white" :background "black"))))
  (face-spec-set 'w3m-tab-background '((t (:background "white" :underline t))))
  (face-spec-set 'w3m-tab-selected '((t (:background "orange"))))
  (face-spec-set 'w3m-tab-selected-retrieving '((t (:background "orange"))))
  (face-spec-set 'w3m-tab-unselected '((t (:background "white"))))
  (face-spec-set 'w3m-tab-unselected-retrieving '((t (:background "white"))))
  (face-spec-set 'w3m-tab-unselected-unseen '((t (:background "white")))))

(use-package dumb-jump
  :if (or (executable-find "ag")
          (executable-find "rg"))
  :bind (("M-=" . dumb-jump-go)
         ("M-\\" . dumb-jump-back)))

(use-package flycheck
  :bind (([f7] . flycheck-mode))
  :config
  (require 'taemin-flycheck)
  (taemin-flycheck-change-keymap-prefix (kbd "C-c l"))
  (taemin-flycheck-select-after-list-errors t))

(use-package glsl-mode
  :mode "\\.\\(glsl\\|vert\\|frag\\)\\'"
  :config
  (add-hook 'glsl-mode-hook (lambda () (c-set-style "stroustrup"))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(rjsx-mode tide web-mode hydra typescript-mode yaml-mode nginx-mode diminish dante ivy-xref dumb-jump w3m undo-fu goto-last-change expand-region markdown-mode haskell-mode anaconda-mode which-key cider adjust-parens avy flx rainbow-delimiters geiser paredit company pcre2el glsl-mode magit smex use-package json-mode js2-mode csharp-mode counsel ag vue-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-blue ((t (:background "dodgerblue" :foreground "dodgerblue"))))
 '(ansi-color-bright-blue ((t (:background "deepskyblue" :foreground "deepskyblue"))))
 '(ansi-color-bright-green ((t (:background "greenyellow" :foreground "greenyellow"))))
 '(ansi-color-bright-red ((t (:background "hotpink" :foreground "hotpink"))))
 '(ansi-color-bright-yellow ((t (:background "gold" :foreground "gold"))))
 '(ansi-color-green ((t (:background "lawngreen" :foreground "lawngreen"))))
 '(ansi-color-red ((t (:background "deeppink" :foreground "deeppink"))))
 '(ansi-color-yellow ((t (:background "orange" :foreground "orange"))))
 '(mode-line ((t (:foreground "white" :background "grey30"))))
 '(mode-line-inactive ((t (:weight light :foreground "grey40" :background "unspecified-bg" :inherit mode-line)))))
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
