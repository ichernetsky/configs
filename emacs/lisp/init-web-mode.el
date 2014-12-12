(require 'web-mode)

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))

(setq web-mode-markup-indent-offset 4)
(setq web-mode-css-indent-offset 4)
(setq web-mode-css-indent-offset 4)
(setq web-mode-style-padding 4)
(setq web-mode-script-padding 4)
(setq web-mode-script-padding 4)

(provide 'init-web-mode)