(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'package)

(add-to-list 'load-path (expand-file-name "elpa" user-emacs-directory))

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

(setq load-prefer-newer t)
(setq package-selected-packages '(erlang))

(package-initialize)
(package-install-selected-packages)

(require 'conf-mac)
(require 'conf-el-get)

(require 'conf-basic)
(require 'conf-helm)

(require 'conf-visual)

(require 'conf-elisp)
(require 'conf-lisp)
(require 'conf-c)
(require 'conf-cc)
(require 'conf-clj)

(provide 'init)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (queue erlang))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
