(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)

;; make a shell script executable automatically on save
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

(provide 'conf-sh)
