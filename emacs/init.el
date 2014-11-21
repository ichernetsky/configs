(when (memq window-system '(mac ns))
  (add-to-list 'load-path "/Applications/Emacs.app/Contents/Resources/lisp/emacs-lisp"))
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'conf-el-get)
(require 'conf-basic)
(require 'conf-ido)

(setq el-get-sources
      '((:name distel
               :info nil
               :build/darwin (list (list "make" (format "EMACS=%s" el-get-emacs) "clean" "base")))))

;; (setq dim-packages
;;       '(cmake-mode magit puppet-mode clojure-mode less-css-mode))

(el-get 'sync '(exec-path-from-shell))
(el-get 'sync '(idomenu smex elisp-slime-nav auto-compile hl-sexp rainbow-delimiters highlight-chars aggressive-indent-mode macrostep paredit markdown-mode web-mode erlang-mode distel cucumber))

(require 'conf-lisp)
(require 'conf-c)

;; (setq user-full-name "Ivan Chernetsky")
;; (setq gnus-select-method '(nntp "news.sunsite.dk"))

;; (setq browse-url-browser-function 'browse-url-generic
;;       browse-url-generic-program "chromium")

;; (add-hook 'dired-load-hook
;;           (function (lambda () (load "dired-x"))))

(require 'server)
(unless (server-running-p)
  (server-start))

(provide 'init)
