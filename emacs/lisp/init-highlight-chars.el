(require 'highlight-chars)

(dolist (mode '(hc-highlight-tabs
                hc-highlight-hard-spaces
                hc-highlight-hard-hyphens
                hc-highlight-trailing-whitespace))
  (add-hook 'prog-mode-hook mode))

(provide 'init-highlight-chars)
