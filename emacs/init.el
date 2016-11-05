(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'package)

(add-to-list 'load-path (expand-file-name "elpa" user-emacs-directory))

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

(setq load-prefer-newer t)
(setq package-selected-packages '(erlang))

(package-initialize)

(require 'conf-mac)
(require 'conf-basic)
(require 'conf-el-get)
(require 'conf-ido)

(require 'conf-visual)

(require 'conf-lisp)
(require 'conf-c)
(require 'conf-clj)
(require 'conf-org)

(provide 'init)
