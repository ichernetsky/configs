(projectile-mode t)

(setq-default projectile-mode-line
              '(:eval (format " prj[%s]" (projectile-project-name))))

(provide 'init-projectile)
