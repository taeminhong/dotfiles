(defun taemin-advice-haskell-load-prompt (orig-fun &rest args)
  "Prompt target on starting REPL"
  (let ((haskell-process-load-or-reload-prompt t))
    (apply orig-fun args)))

(provide 'taemin-haskell)
