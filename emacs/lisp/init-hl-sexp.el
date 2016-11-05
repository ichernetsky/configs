(require 'hl-sexp)

(defadvice hl-sexp-mode (after unflicker (&optional turn-on) activate)
  (when turn-on
    (remove-hook 'pre-command-hook #'hl-sexp-unhighlight)))

(provide 'init-hl-sexp)
