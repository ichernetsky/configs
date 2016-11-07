(global-visual-line-mode t)

(setq-default initial-scratch-message nil)

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
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode 0))
(scroll-bar-mode 0)

(setq ring-bell-function 'ignore)

(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")
(setq uniquify-separator "/")

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(global-font-lock-mode t)
(show-paren-mode t)

(fset 'yes-or-no-p 'y-or-n-p)
(setq-default regex-tool-backend 'perl)

(global-subword-mode t)

(require 'tramp)
(setq tramp-ssh-controlmaster-options
      (concat
       "-o ControlPath=/tmp/ssh-ControlPath-%%r@%%h:%%p "
       "-o ControlMaster=auto -o ControlPersist=no"))

(setq user-full-name "Ivan Chernetsky")
(setq gnus-select-method '(nntp "news.sunsite.dk"))

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "chromium")

(delete-selection-mode t)

;; smart tab behavior - indent or complete
(setq tab-always-indent 'complete)

(require 'recentf)
(setq recentf-max-saved-items 500
      recentf-max-menu-items 15
      ;; disable recentf-cleanup on Emacs start, because it can cause
      ;; problems with remote files
      recentf-auto-cleanup 'never)
(recentf-mode t)

(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(put 'erase-buffer 'disabled nil)

(put 'dired-find-alternate-file 'disabled nil)
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)
(setq dired-dwim-target t)
(require 'dired-x)

(require 'ediff)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; clean up obsolete buffers automatically
(require 'midnight)

(require 'tabify)

;; make a shell script executable automatically on save
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; saner regex syntax
(require 're-builder)
(setq reb-re-syntax 'string)

(require 'eshell)

(require 'compile)
(setq compilation-ask-about-save nil
      compilation-always-kill t
      compilation-scroll-output 'first-error)

(require 'which-func)
(which-function-mode t)

(require 'server)
(unless (server-running-p)
  (server-start))

(provide 'conf-basic)
