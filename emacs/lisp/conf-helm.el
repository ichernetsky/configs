(require 'helm-config)
(require 'helm-descbinds)
(require 'helm-eshell)
(require 'helm-projectile)

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t
      helm-buffers-fuzzy-matching           t
      helm-move-to-line-cycle-in-source     t
      helm-ff-search-library-in-sexp        t
      helm-ff-file-name-history-use-recentf t)

(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-command-map (kbd "o")     'helm-occur)
(define-key helm-command-map (kbd "g")     'helm-do-grep)
(define-key helm-command-map (kbd "C-c w") 'helm-wikipedia-suggest)
(define-key helm-command-map (kbd "SPC")   'helm-all-mark-rings)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-m") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-h f") 'helm-apropos)
(global-set-key (kbd "C-h r") 'helm-info-emacs)
(global-set-key (kbd "C-h C-l") 'helm-locate-library)
(global-set-key (kbd "C-c f") 'helm-recentf)

(define-key global-map [remap find-tag] 'helm-etags-select)
(substitute-key-definition 'find-tag 'helm-etags-select global-map)

(define-key shell-mode-map (kbd "C-c C-l") 'helm-comint-input-ring)

(add-hook 'eshell-mode-hook
          #'(lambda ()
              (substitute-key-definition 'eshell-list-history 'helm-eshell-history
                                         eshell-mode-map)))

(helm-mode t)
(helm-descbinds-mode t)
(helm-projectile-on)

(provide 'conf-helm)
