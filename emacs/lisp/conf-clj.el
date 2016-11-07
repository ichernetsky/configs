(defun conf-clj/hook-fn ()
  (rainbow-delimiters-mode t)
  (enable-paredit-mode)
  (turn-on-eldoc-mode)
  (add-hook 'after-save-hook #'check-parens nil t))

(dolist (hook '(clojure-mode-hook cider-mode-hook cider-repl-mode-hook))
  (add-hook hook 'conf-clj/hook-fn))

(add-hook 'cider-repl-mode-hook #'company-mode)
(add-hook 'cider-mode-hook #'company-mode)

(provide 'conf-clj)
