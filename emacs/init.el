(require 'site-gentoo "site-gentoo.el" t)

(add-to-list 'load-path "~/.emacs.d/site-lisp/")

(set-default-font "-xos4-terminus-bold-*-*-*-14-*-*-*-*-*-iso10646-1")

(if (require 'zenburn "zenburn.el" t)
    (color-theme-zenburn))

;; do not make backup files
(setq make-backup-files nil)
(auto-save-mode nil)
;; remove the splash screen at startup
(setq inhibit-splash-screen t)
;; make emacs ask about missing newline
(setq require-final-newline 'ask)
(setq-default indent-tabs-mode nil)

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; disable unnessary clutter
(menu-bar-mode nil)
(tool-bar-mode nil)
(scroll-bar-mode nil)

(global-font-lock-mode t)
(ido-mode t)
(show-paren-mode t)
(column-number-mode t)

(global-visual-line-mode t)
(iswitchb-mode t)

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "chromium"
      browse-url-generic-args '("--incognito"))

(if (require 'edit-server "edit-server.el" t)
    (edit-server-start))

(if (require 'show-wspace "show-wspace.el" t)
    (add-hook 'c-mode-hook
              (lambda ()
                (show-ws-highlight-tabs)
                (show-ws-highlight-hard-spaces)
                (show-ws-highlight-trailing-whitespace))))

(setq c-basic-offset 4)
(setq c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
                        (other . "k&r")))

(add-hook 'c-initialization-hook
          (lambda ()
            (define-key c-mode-base-map "\C-m" 'c-context-line-break)))

(setq inferior-lisp-program "/usr/bin/sbcl")

(if (file-exists-p "/usr/share/emacs/site-lisp/slime")
    (progn
      (add-to-list 'load-path
                   "/usr/share/emacs/site-lisp/slime")
      (require 'slime-autoloads)
      (slime-setup '(slime-fancy slime-mrepl slime-scratch
                                 slime-asdf slime-banner
                                 slime-xref-browser))
      (setq slime-net-coding-system 'utf-8-unix)
      (setq slime-use-autodoc-mode nil)
      (setq common-lisp-hyperspec-root
            (if (file-exists-p "/usr/share/doc/hyperspec/HyperSpec")
                "file:///usr/share/doc/hyperspec/HyperSpec/"
              "http://www.lispworks.com/reference/HyperSpec/"))
      (setq slime-scratch-file "~/.emacs.d/slime-scratch")))

(setq user-full-name "Ivan Chernetsky")
(setq gnus-select-method '(nntp "news.sunsite.dk"))

(if (file-exists-p "~/dev/clojure-mode")
    (progn
      (add-to-list 'load-path "~/dev/clojure-mode/")
      (require 'clojure-mode)
      (add-hook 'slime-repl-mode-hook 'clojure-mode-font-lock-setup)))

(if (file-executable-p "~/bin/xref")
    (progn
      (setq xref-dir "~/bin/xref/")
      (add-to-list 'exec-path xref-dir)
      (add-to-list 'load-path (concat xref-dir "emacs/"))
      (require 'xrefactory "xrefactory.el" t)))

(if (require 'haskell-mode "haskell-mode.el" t)
    (add-hook 'haskell-mode-hook
              (lambda ()
                (turn-on-haskell-doc-mode)
                (turn-on-haskell-indent)
                (turn-on-haskell-font-lock)
                (turn-on-haskell-decl-scan)
                (turn-on-haskell-indentation)
                (local-set-key [return] 'newline-and-indent)
                (local-set-key "\C-ch" 'haskell-hoogle)
                (local-set-key "\C-c\C-h" 'haskell-hayoo)
                (setq haskell-font-lock-symbols t))))

(if (require 'cmake-mode "cmake-mode.el" t)
    (setq auto-mode-alist
          (append '(("CMakeLists\\.txt\\'" . cmake-mode)
                    ("\\.cmake\\'" . cmake-mode))
                  auto-mode-alist)))

(if (file-exists-p "~/.emacs.d/init-local.el")
    (load "~/.emacs.d/init-local.el"))

(if (file-exists-p "~/dev/distel")
    (progn
      (add-to-list 'load-path "~/dev/distel/elisp/")
      (require 'distel)
      (distel-setup)))

(if (require 'markdown-mode "markdown-mode.el" t)
    (setq auto-mode-alist
          (cons '("\\.md" . markdown-mode) auto-mode-alist)))

(if (file-exists-p "~/dev/geiser/build/elisp/geiser-load.el")
    (load "~/dev/geiser/build/elisp/geiser-load"))
