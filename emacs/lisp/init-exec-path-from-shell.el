(require 'exec-path-from-shell)

(dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE"))
  (add-to-list 'exec-path-from-shell-variables var))

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (setenv "PATH" (format "%s:%s"
                         (getenv "PATH")
                         "/usr/local/bin:/usr/local/git/bin"))
  (setq exec-path (split-string (getenv "PATH") path-separator)))

(provide 'init-exec-path-from-shell)
