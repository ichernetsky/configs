(require 'highlight-chars)

(dolist (mode '(hc-highlight-tabs
                hc-highlight-hard-spaces
                hc-highlight-hard-hyphens
                hc-highlight-trailing-whitespace))
  (add-hook 'prog-mode-hook mode))

(add-hook 'makefile-mode-hook (lambda ()
                                (hc-dont-highlight-tabs)))

(provide 'init-highlight-chars)
