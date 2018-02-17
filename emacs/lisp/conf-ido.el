(require 'ido)

(setq-default ido-enable-flex-matching t)
(setq-default ido-everywhere t)
(setq-default ido-enable-last-directory-history t)
(setq-default ido-enable-case nil)
(setq-default ido-enable-prefix nil)
(ido-mode t)

(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file."
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

(global-set-key (kbd "C-x C-r") 'ido-recentf-open)

(require 'smex)
(smex-initialize)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(provide 'conf-ido)
