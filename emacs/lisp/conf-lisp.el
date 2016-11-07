(defun conf-lisp/hook-fn ()
  (rainbow-delimiters-mode t)
  (enable-paredit-mode)
  (turn-on-eldoc-mode)
  (add-hook 'after-save-hook #'check-parens nil t))

(dolist (hook '(lisp-mode-hook inferior-lisp-mode-hook
                               lisp-interaction-mode-hook))
  (add-hook hook 'conf-lisp/hook-fn))

(provide 'conf-lisp)
