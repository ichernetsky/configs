(require 'site-gentoo "site-gentoo.el" t)

(add-to-list 'load-path "~/.emacs.d/site-lisp/")

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

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "chromium")

(if (require 'edit-server "edit-server.el" t)
    (edit-server-start))

(setq inferior-lisp-program "/usr/bin/sbcl")

(if (not (member 'slime features))
    (progn
      (add-to-list 'load-path
                   "/usr/share/emacs/site-lisp/slime")
      (if (require 'slime-autoloads "slime-autoloads.el" t)
          (slime-setup '(slime-fancy slime-mrepl
                                     slime-asdf slime-banner
                                     slime-xref-browser)))))

(if (member 'slime features)
    (progn
      (setq slime-net-coding-system 'utf-8-unix)
      (setq slime-use-autodoc-mode nil)
      (setq common-lisp-hyperspec-root
            (if (file-exists-p "/usr/share/doc/hyperspec/HyperSpec")
                "file:///usr/share/doc/hyperspec/HyperSpec/"
              "http://www.lispworks.com/reference/HyperSpec/"))))

(setq user-full-name "Ivan Chernetsky")
(setq gnus-select-method '(nntp "news.sunsite.dk"))

(if (file-exists-p "~/dev/clojure-mode")
    (progn
      (add-to-list 'load-path "~/dev/clojure-mode/")
      (require 'clojure-mode)))

(if (file-executable-p "~/bin/xref")
    (progn
      (setq xref-dir "~/bin/xref/")
      (add-to-list 'exec-path xref-dir)
      (add-to-list 'load-path (concat xref-dir "emacs/"))
      (require 'xrefactory "xrefactory.el" t)))

(if (member 'haskell-mode features)
    (add-hook 'haskell-mode-hook
              (lambda ()
                (turn-on-haskell-doc-mode)
                (turn-on-haskell-indent)
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
