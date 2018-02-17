(set-face-attribute 'default t :font "Fira Code Retina")
(set-face-attribute 'default nil :height 160)
(load-theme 'dracula t)

;; display available keybindings in a popup
(require 'which-key)
(which-key-mode t)

;; diminish keeps the modeline tidy
(require 'diminish)
(add-hook 'which-key-mode-hook
          (lambda () (diminish 'which-key-mode)))
(add-hook 'visual-line-mode-hook
          (lambda () (diminish 'visual-line-mode)))
(add-hook 'subword-mode-hook
          (lambda () (diminish 'subword-mode)))
(add-hook 'auto-revert-mode-hook
          (lambda () (diminish 'auto-revert-mode)))
(add-hook 'eldoc-mode-hook
          (lambda () (diminish 'eldoc-mode)))
(add-hook 'elisp-slime-nav-mode-hook
          (lambda () (diminish 'elisp-slime-nav-mode)))
(add-hook 'paredit-mode-hook
          (lambda () (diminish 'paredit-mode)))
(add-hook 'undo-tree-mode-hook
          (lambda () (diminish 'undo-tree-mode)))

(global-undo-tree-mode)

(provide 'conf-visual)
