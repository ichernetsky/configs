(require 'magit)

(setq-default magit-save-some-buffers nil)
(setq-default magit-process-popup-time 10)
(setq-default magit-diff-refine-hunk t)
(setq-default magit-completing-read-function 'magit-ido-completing-read)

(define-key magit-status-mode-map (kbd "C-M-<up>") 'magit-goto-parent-section)
(add-hook 'git-commit-mode-hook 'goto-address-mode)

(provide 'init-magit)
