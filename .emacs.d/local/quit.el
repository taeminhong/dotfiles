;;; C-q to cancel whatever
(global-set-key (kbd "C-q") 'keyboard-quit)
(define-key ivy-minibuffer-map (kbd "C-q") 'minibuffer-keyboard-quit)
(add-to-list 'avy-escape-chars ?\C-q)
