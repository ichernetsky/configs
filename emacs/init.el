(if (eq system-type 'darwin)
  (setenv "PATH" (format "%s:%s:%s"
                         (getenv "PATH")
                         "/usr/local/git/bin")))
(setq exec-path (split-string (getenv "PATH") path-separator))

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))

(setq el-get-sources
      '((:name paredit
               :after (progn (autoload 'enable-paredit-mode "paredit"
                               "Turn on pseudo-structural editing of Lisp code." t)
                             (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
                             (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
                             (add-hook 'ielm-mode-hook #'enable-paredit-mode)
                             (add-hook 'lisp-mode-hook #'enable-paredit-mode)
                             (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
                             (add-hook 'scheme-mode-hook #'enable-paredit-mode)
                             (require 'eldoc)
                             (eldoc-add-command
                              'paredit-backward-delete
                              'paredit-close-round)))))

(setq dim-packages
      '(paredit markdown-mode cmake-mode erlware-mode magit puppet-mode web-mode
        clojure-mode less-css-mode highlight-chars))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(el-get 'sync dim-packages)

;; do not make backup files
(setq backup-inhibited t)
(setq auto-save-default nil)
(setq make-backup-files nil)
;; remove the splash screen at startup
(setq inhibit-splash-screen t)
;; make emacs ask about missing newline
(setq require-final-newline 'ask)
(setq-default indent-tabs-mode nil)

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; disable unnessary clutter
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

(global-font-lock-mode t)
(ido-mode t)
(show-paren-mode t)
(column-number-mode t)

(global-visual-line-mode t)
(iswitchb-mode t)

(setq c-basic-offset 4)
(setq c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
                        (other . "k&r")))

(setq user-full-name "Ivan Chernetsky")
(setq gnus-select-method '(nntp "news.sunsite.dk"))

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "chromium")

(setq ring-bell-function 'ignore)

(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

(add-hook 'dired-load-hook
	  (function (lambda () (load "dired-x"))))

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(setq web-mode-markup-indent-offset 4)
(setq web-mode-css-indent-offset 4)
(setq web-mode-css-indent-offset 4)
(setq web-mode-style-padding 4)
(setq web-mode-script-padding 4)
(setq web-mode-script-padding 4)

(require 'highlight-chars)
(add-hook 'font-lock-mode-hook 'hc-highlight-tabs)
(add-hook 'font-lock-mode-hook 'hc-highlight-hard-spaces)
(add-hook 'font-lock-mode-hook 'hc-highlight-hard-hyphens)
(add-hook 'font-lock-mode-hook 'hc-highlight-trailing-whitespace)
