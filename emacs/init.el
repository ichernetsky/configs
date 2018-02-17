(package-initialize)

(setq-default package-enable-at-startup nil)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(prefer-coding-system 'utf-8)

(require 'conf-mac)
(require 'conf-el-get)

(require 'conf-basic)
(require 'conf-visual)
(require 'conf-ido)

(require 'conf-sh)
(require 'conf-elisp)
(require 'conf-lisp)
(require 'conf-c)
(require 'conf-clj)
(require 'conf-erlang)

(provide 'init)
