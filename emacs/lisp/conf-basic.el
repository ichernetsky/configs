(global-visual-line-mode t)

(setq-default initial-scratch-message "")

;; do not make backup files
(setq backup-inhibited t)
(setq auto-save-default nil)
(setq make-backup-files nil)

;; remove the splash screen at startup
(setq inhibit-splash-screen t)

;; make emacs ask about missing newline
(setq require-final-newline 'ask)
(setq-default indent-tabs-mode nil)

;; disable unnessary clutter
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

(setq ring-bell-function 'ignore)

(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(global-font-lock-mode t)
(show-paren-mode t)
(column-number-mode t)

(fset 'yes-or-no-p 'y-or-n-p)
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
(setq goto-address-mail-face 'link)
(setq-default regex-tool-backend 'perl)

(global-subword-mode t)

(provide 'conf-basic)
