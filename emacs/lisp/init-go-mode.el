(require 'highlight-chars)

(add-hook 'go-mode-hook (lambda ()
                          (hc-dont-highlight-tabs)))

(provide 'init-go-mode)
