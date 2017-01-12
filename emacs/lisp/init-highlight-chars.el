(require 'highlight-chars)

(dolist (mode '(hc-highlight-tabs
                hc-highlight-hard-spaces
                hc-highlight-hard-hyphens
                hc-highlight-trailing-whitespace))
  (dolist (hook '(prog-mode-hook
                  markdown-mode-hook
                  rst-mode-hook))
    (add-hook hook mode)))

(add-hook 'makefile-mode-hook (lambda ()
                                (hc-dont-highlight-tabs)))

(provide 'init-highlight-chars)
