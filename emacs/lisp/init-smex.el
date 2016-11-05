(require 'smex)

(setq smex-save-file
      (expand-file-name ".smex-items" user-emacs-directory))

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; old M-x
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(provide 'init-smex)
