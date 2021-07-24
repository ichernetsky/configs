;; set a preferred coding system everywhere
(prefer-coding-system 'utf-8)

;; do not activate installed packages when emacs starts
(setq-default package-enable-at-startup nil)

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

;; remove the splash screen at start-up
(setq-default inhibit-splash-screen t)

;; make emacs ask about a missing newline
(setq-default require-final-newline 'ask)

;; use spaces for indentation
(setq-default indent-tabs-mode nil)

;; disable useless clutter
(menu-bar-mode 0)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode 0))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode 0))

;; make less noise
(setq-default ring-bell-function 'ignore)

;; make scrolling work in a less surprising way
(setq-default scroll-margin 0)
(setq-default scroll-conservatively 100000)
(setq-default scroll-preserve-screen-position 1)

;; show the line and column numbers
(line-number-mode 1)
(column-number-mode 1)

;; show a file size in the mode line
(size-indication-mode 1)

;; make buffer names unique in the mode line
(require 'uniquify)
(setq-default uniquify-buffer-name-style 'reverse)
(setq-default uniquify-after-kill-buffer-p t)
(setq-default uniquify-ignore-buffers-re "^\\*")
(setq-default uniquify-separator "/")

;; select buffers geometrically with ctrl+arrow
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; syntax highlighting everywhere
(global-font-lock-mode 1)

;; highlight matching parens
(show-paren-mode 1)

;; be laconic with yes-or-no questions
(fset 'yes-or-no-p 'y-or-n-p)

;; fix trailing whitespaces on file save
(add-hook 'before-save-hook #'whitespace-cleanup)

;; replace a selection with what being typed
(delete-selection-mode 1)

;; remember files visited recently
(require 'recentf)
(setq-default recentf-max-saved-items 500)
(setq-default recentf-max-menu-items 15)
;; disable recentf-cleanup on emacs start, because it can cause
;; problems with remote files
(setq-default recentf-auto-cleanup 'never)
(recentf-mode 1)

;; diffs are always handy
(require 'ediff)
(setq-default ediff-window-setup-function 'ediff-setup-windows-plain)

;; show the current function in the mode line
(which-function-mode 1)

;; something to keep in mind
(setq-default user-full-name "Ivan Chernetsky")

;; we know what we are doing
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)

;; remote file editing
(require 'tramp)
(cond
 ((string-equal system-type "darwin")
  (setq-default tramp-ssh-controlmaster-options
                (concat
                 "-o ControlPath=/tmp/ssh-ControlPath-%%r@%%h:%%p "
                 "-o ControlMaster=auto -o ControlPersist=no"))))

;; we want to use emacsclient
(require 'server)
(unless (server-running-p)
  (server-start))

;; install straight.el package manager
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
;; make use-package use straight by default
(setq-default straight-use-package-by-default t)

(defmacro use-feature (name &rest args)
  "Like `use-package' for NAME and ARGS, but with `:straight' nil."
  (declare (indent defun))
  `(use-package ,name
     :straight nil
     ,@args))

;; keep the mode bar tidy
(use-package delight)

;; spaces and punctuation are not only word boundaries
(use-feature subword
  :delight)
(global-subword-mode 1)

;; set file search paths
(use-package exec-path-from-shell
  :custom
  (exec-path-from-shell-check-startup-files nil)
  :config
  (exec-path-from-shell-initialize))
(dolist (path (list "/usr/local/bin/" (expand-file-name "~/bin/")))
  (unless (member path exec-path)
    (add-to-list 'exec-path path)))

;; web browser settings
(setq-default browse-url-text-browser (executable-find "lynx"))
(setq-default browse-url-browser-function
              (cond
               ((and (display-graphic-p)
                     (string-equal system-type "darwin"))
                #'browse-url-default-macosx-browser)
               ((not (display-graphic-p))
                #'browse-url-text-emacs)
               (t #'browse-url-default-browser)))

(use-feature emacs
  :delight
  (abbrev-mode)
  (auto-revert-mode)
  (subword-mode)
  (visual-line-mode))

;; display available key bindings in a pop-up
(use-package which-key
  :delight
  :config
  (which-key-mode 1))

(use-feature eldoc
  :delight)

;; magit auto-completion needs it
;; (use-package ido-completing-read+)

;; a git porcelain inside emacs
(use-package magit
  :init
  (add-hook 'git-commit-mode-hook #'goto-address-mode)
  :custom
  (magit-process-popup-time 10)
  (magit-diff-refine-hunk t)
  ;; (magit-completing-read-function 'magit-ido-completing-read)
  :config
  (define-key magit-status-mode-map (kbd "C-M-<up>") 'magit-goto-parent-section))

;; (setq-default completion-styles '(flex))

(use-package helm
  :delight
  :init
  :config
  (global-unset-key (kbd "C-x c"))
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "C-x C-d") 'helm-browse-project)
  (global-set-key (kbd "C-x r p") 'helm-projects-history)
  (global-set-key (kbd "C-x b") 'helm-mini)
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)
  (helm-autoresize-mode 1)
  (helm-mode 1)
  :custom
  (helm-M-x-fuzzy-match t)
  (helm-buffers-fuzzy-matching t)
  (helm-recentf-fuzzy-match t)

  (helm-split-window-inside-p t)
  (helm-move-to-line-cycle-in-source t)

  (helm-ff-search-library-in-sexp t)
  (helm-ff-file-name-history-use-recentf t)

  (helm-autoresize-max-height 0)
  (helm-autoresize-min-height 30)

  (helm-commands-using-frame '(completion-at-point
                               helm-apropos
                               helm-eshell-prompts
                               helm-imenu
                               helm-imenu-in-all-buffers)))

(use-feature helm-adaptive
  :custom
  (helm-adaptive-history-file nil)
  :config
  (helm-adaptive-mode 1))

(use-feature helm-sys
  :commands
  (helm-top)
  :config
  (helm-top-poll-mode 1))

(use-feature helm-info
  :bind ("C-h r" . helm-info-emacs))

(use-feature helm-eshell
  :config
  (add-hook 'eshell-mode-hook
            (lambda ()
              (define-key eshell-mode-map (kbd "C-c C-l") 'helm-eshell-history)))
  (define-key shell-mode-map (kbd "C-c C-l") 'helm-comint-input-ring)
  (define-key minibuffer-local-map (kbd "C-c C-l") 'helm-minibuffer-history))

(use-package helm-ls-git
  :custom
  (helm-ls-git-status-command 'magit-status-setup-buffer))

(use-package helm-ag
  :custom
  (helm-ag-base-command "rg --no-heading")
  (helm-ag-success-exit-status '(0 2)))

;; project management and navigation
(use-package projectile
  :config
  (projectile-global-mode 1)
  :custom
  (projectile-mode-line-function
   '(lambda () (format " prj[%s]" (projectile-project-name))))
  (projectile-completion-system 'helm))

(use-package helm-projectile
  :config
  (helm-projectile-on))

;; (defun ido-recentf-open ()
;;   "Use `ido-completing-read' to \\[find-file] a recent file."
;;   (interactive)
;;   (if (find-file (ido-completing-read "Find recent file: " recentf-list))
;;       (message "Opening file...")
;;     (message "Aborting")))

;; (use-package ido
;;   :commands
;;   (ido-mode)
;;   :init
;;   (ido-mode 1)
;;   :custom
;;   (ido-enable-flex-matching t)
;;   (ido-everywhere t)
;;   (ido-enable-last-directory-history t)
;;   (ido-enable-prefix nil)
;;   :bind
;;   ("C-x C-r" . ido-recentf-open))

;; nice interface to recently and frequently used commands on top of ido
;; (use-package smex
;;   :config
;;   (smex-initialize)
;;   :bind
;;   (("M-x" . smex)
;;    ("M-X" . smex-major-mode-commands)))

;; highlight irritating whitespaces
(use-package highlight-chars)

(defun hc-setup ()
  (hc-highlight-tabs)
  (hc-highlight-trailing-whitespace)
  (hc-highlight-hard-spaces)
  (hc-highlight-hard-hyphens))

(add-hook 'prog-mode-hook #'hc-setup)
(add-hook 'makefile-mode-hook #'hc-dont-highlight-tabs)
(add-hook 'conf-mode-hook #'hc-setup)

;; interface to ripgrep
(use-package rg)

;; some generic compilation options
(setq-default compilation-ask-about-save nil)
(setq-default compilation-always-kill t)
(setq-default compilation-scroll-output 'first-error)

;; superior way to edit s-exps
(use-package paredit
  :delight)

;; automatically compile emacs lisp files
(use-package auto-compile
  :config
  (auto-compile-on-save-mode t)
  (auto-compile-on-load-mode t))

;; slime-style navigation for emacs lisp source code
(use-package elisp-slime-nav
  :delight)

;; highlight parens and all according to their nesting level
(use-package rainbow-delimiters)

;; highlight the s-exp at the current position
(use-package hl-sexp
  :commands
  hl-sexp-unhighlight
  :init
  (defadvice hl-sexp-mode (after unflicker (&optional turn-on) activate)
    (when turn-on
      (remove-hook 'pre-command-hook #'hl-sexp-unhighlight))))

;; elisp configurations
(use-feature ielm)

(defvar elisp-repl-original-buffer nil
  "Buffer from which we jumped to this REPL.")
(make-variable-buffer-local 'elisp-repl-original-buffer)
(defvar elisp-repl-switch-function #'switch-to-buffer-other-window)

(defun elisp-switch-to-ielm ()
  (interactive)
  (let ((orig-buffer (current-buffer)))
    (if (get-buffer "*ielm*")
        (funcall elisp-repl-switch-function "*ielm*")
      (ielm))
    (setq elisp-repl-original-buffer orig-buffer)))

(defun elisp-repl-switch-back ()
  "Switch back to the buffer from which we reached this REPL."
  (interactive)
  (if elisp-repl-original-buffer
      (funcall elisp-repl-switch-function elisp-repl-original-buffer)
    (error "No original buffer.")))

(define-key emacs-lisp-mode-map (kbd "C-c C-z") #'elisp-switch-to-ielm)
(define-key ielm-map (kbd "C-c C-z") #'elisp-repl-switch-back)

(defun elisp-hook-fn ()
  "Enable features useful when working with elisp."
  (rainbow-delimiters-mode 1)
  (enable-paredit-mode)
  (turn-on-eldoc-mode)
  (add-hook 'after-save-hook #'check-parens nil t)
  (elisp-slime-nav-mode 1))

(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook #'elisp-hook-fn))

(eldoc-add-command
 #'paredit-backward-delete
 #'paredit-close-round)

;; lisp configurations
(defun lisp-hook-fn ()
  (rainbow-delimiters-mode 1)
  (enable-paredit-mode)
  (turn-on-eldoc-mode)
  (add-hook 'after-save-hook #'check-parens nil t))

(dolist (hook '(lisp-mode-hook inferior-lisp-mode-hook
                               lisp-interaction-mode-hook))
  (add-hook hook #'lisp-hook-fn))

;; shell script mode options
(setq-default sh-basic-offset 4)

;; make scripts executable automatically on save
(add-hook 'after-save-hook
          #'executable-make-buffer-file-executable-if-script-p)

;; c/c++ mode options
(setq-default c-default-style '((java-mode . "java")
                                (awk-mode . "awk")
                                (other . "k&r")))

;; cmake files
(use-package cmake-mode)

;; golang files
(use-package go-mode)

(add-hook 'go-mode-hook #'hc-dont-highlight-tabs)

;; protobufs
(use-package protobuf-mode)

;; yaml files
(use-package yaml-mode)

;; markdown files
(use-package markdown-mode)

;; docker image files
(use-package dockerfile-mode)

;; terraform configuration files
(use-package terraform-mode)

;; web templates, html, css, and js files
(use-package web-mode
  :mode
  ("\\.html?\\'" "\\.css\\'")
  :custom
  (web-mode-markup-indent-offset 4)
  (web-mode-css-indent-offset 4)
  (web-mode-style-padding 4)
  (web-mode-script-padding 4)
  (web-mode-script-padding 4))

(defun clj-mode-hook-fn ()
  (rainbow-delimiters-mode t)
  (enable-paredit-mode)
  (turn-on-eldoc-mode)
  (add-hook 'after-save-hook #'check-parens nil t))

;; clojure
(use-package clojure-mode
  :hook
  (clojure-mode . #'clj-modehook-fn))

;; clojure interactive development environment that rocks
(use-package cider
  :hook
  ((cider-mode . #'clj-modehook-fn)
   (cider-repl-mode . #'clj-modehook-fn)))

;; erlang
(use-package erlang
  :init
  ;; prevent annoying hang-on-compile
  (setq-default inferior-erlang-prompt-timeout t)
  ;; default node name to emacs@localhost
  (setq-default inferior-erlang-machine-options '("-name" "emacs@127.0.0.1")))

;; distributed emacs lisp for erlang
;; (use-package distel)

;; frontend to gnu global
(use-package ggtags)

(use-package company
  :delight
  :config
  (global-company-mode 1))

(use-package groovy-mode)

;; 100 MB
(setq gc-cons-threshold (* 100 1024 1024))
;; 1 MB
(setq read-process-output-max (* 1 1024 1024))

(use-package lsp-mode
  :config
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]vendor\\'")
  :commands
  (lsp lsp-deferred)
  :hook
  (c-mode-common . lsp-deferred)
  (go-mode . lsp-deferred)
  :custom
  (lsp-file-watch-threshold 2000)
  (lsp-enable-indentation nil)
  (lsp-enable-on-type-formatting nil)
  (lsp-enable-snippet nil))

(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

(use-package lsp-ui
  :commands
  lsp-ui-mode)

(use-package treemacs)
(use-package treemacs-projectile)
(use-package treemacs-magit)
(use-package lsp-treemacs)

(use-package helm-lsp)

(use-package company-lsp
  :custom
  (company-lsp-enable-recompletion t)
  :config
  (add-to-list 'company-backends 'company-lsp))

(when (executable-find "hunspell")
  (setq-default ispell-program-name "hunspell")
  (setq ispell-really-hunspell t))

(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))

;; install flycheck
;; company-capf

(dolist (mode '(c-mode-common-hook
                emacs-lisp-mode-hook
                inferior-lisp-mode-hook
                go-mode-hook
                clojure-mode-hook
                dockerfile-mode-hook))
  (add-hook mode (lambda () (flyspell-prog-mode))))

(use-package langtool
  :custom
  langtool-bin "/usr/local/bin/languagetool"
  langtool-mother-tongue "en")

;; some fancy fonts and colors
(when (display-graphic-p)
  (set-face-attribute 'default nil :font "Fira Code Retina")
  (set-face-attribute 'default nil :height 160)
  (use-package espresso-theme))
