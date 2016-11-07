(setq goto-address-mail-face 'link)

(color-theme-tangotango)
(set-face-attribute 'default nil :height 140)

(require 'beacon)
(beacon-mode t)

(require 'which-key)
(which-key-mode t)

;; diminish keeps the modeline tidy
(require 'diminish)

(require 'volatile-highlights)
(volatile-highlights-mode t)
(diminish 'volatile-highlights-mode)

(require 'anzu)
(diminish 'anzu-mode)
(global-anzu-mode)

(global-set-key (kbd "M-%") 'anzu-query-replace)
(global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)

(global-undo-tree-mode)
(diminish 'undo-tree-mode)

(when (fboundp 'winner-mode)
  (winner-mode t))

(provide 'conf-visual)
