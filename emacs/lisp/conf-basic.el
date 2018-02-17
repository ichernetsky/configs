;; maximize emacs on start-up
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; edit visual lines, not logical ones
(global-visual-line-mode t)

;; remove clutter from the initial buffer
(setq-default initial-scratch-message nil)

;; do not make backup files
(setq-default backup-inhibited t)
(setq-default auto-save-default nil)
(setq-default make-backup-files nil)

;; remove the splash screen at startup
(setq-default inhibit-splash-screen t)

;; make emacs ask about missing newline
(setq-default require-final-newline 'ask)

;; use spaces for indentation
(setq-default indent-tabs-mode nil)

;; disable unnessary clutter
(menu-bar-mode 0)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode 0))
(scroll-bar-mode 0)

;; make less noise
(setq-default ring-bell-function 'ignore)

;; make scrolling work in a least surprising way
(setq-default scroll-margin 0)
(setq-default scroll-conservatively 100000)
(setq-default scroll-preserve-screen-position 1)

;; show the line number
(line-number-mode t)

;; and the column one too
(column-number-mode t)

;; sow a file size in the mode line
(size-indication-mode t)

;; make buffer names unique in the mode line
(require 'uniquify)
(setq-default uniquify-buffer-name-style 'reverse)
(setq-default uniquify-after-kill-buffer-p t)
(setq-default uniquify-ignore-buffers-re "^\\*")
(setq-default uniquify-separator "/")

;; select buffers geometrically with Ctrl+Arrow
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; syntax highlighting everywhere
(global-font-lock-mode t)

;; highlight matching parens
(show-paren-mode t)

;; be laconic with questions
(fset 'yes-or-no-p 'y-or-n-p)

;; with power comes responsibility
(setq-default regex-tool-backend 'perl)

;; spaces and punctuation are not only word boundaries
(global-subword-mode t)

;; Fix trailing whitespace on file save.
(add-hook 'before-save-hook 'whitespace-cleanup)

(require 'tramp)
(setq-default tramp-ssh-controlmaster-options
              (concat
               "-o ControlPath=/tmp/ssh-ControlPath-%%r@%%h:%%p "
               "-o ControlMaster=auto -o ControlPersist=no"))

;; replace a selection with what you type
(delete-selection-mode t)

;; remember files visited recently
(require 'recentf)
(setq-default recentf-max-saved-items 500)
(setq-default recentf-max-menu-items 15)
;; disable recentf-cleanup on Emacs start, because it can cause
;; problems with remote files
(setq-default recentf-auto-cleanup 'never)
(recentf-mode t)

;; diffs are always handy
(require 'ediff)
(setq-default ediff-window-setup-function 'ediff-setup-windows-plain)

;; saner regex syntax
(require 're-builder)
(setq-default reb-re-syntax 'string)

(require 'compile)
(setq-default compilation-ask-about-save nil
              compilation-always-kill t
              compilation-scroll-output 'first-error)

;; show current function in the mode line
(which-function-mode t)

;; something to keep in mind
(setq-default user-full-name "Ivan Chernetsky")

;; use Chrome
(setq-default browse-url-browser-function 'browse-url-generic)
(setq-default browse-url-generic-program
              (if (eq window-system 'ns)
                  "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
                  "chromium"))

;; we know what we are doing
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)

(require 'server)
(unless (server-running-p)
  (server-start))

(provide 'conf-basic)
