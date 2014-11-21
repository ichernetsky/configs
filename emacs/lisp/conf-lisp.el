(require 'eldoc)
(require 'ielm)

(defvar conf-lisp/repl-original-buffer nil
  "Buffer from which we jumped to this REPL.")
(make-variable-buffer-local 'conf-lisp/repl-original-buffer)

(defvar conf-lisp/repl-switch-function 'switch-to-buffer-other-window)

(defun conf-lisp/switch-to-ielm ()
  (interactive)
  (let ((orig-buffer (current-buffer)))
    (if (get-buffer "*ielm*")
        (funcall conf-lisp/repl-switch-function "*ielm*")
      (ielm))
    (setq conf-lisp/repl-original-buffer orig-buffer)))

(defun conf-lisp/repl-switch-back ()
  "Switch back to the buffer from which we reached this REPL."
  (interactive)
  (if conf-lisp/repl-original-buffer
      (funcall conf-lisp/repl-switch-function conf-lisp/repl-original-buffer)
    (error "No original buffer.")))

(define-key emacs-lisp-mode-map (kbd "C-c C-z") 'conf-lisp/switch-to-ielm)
(define-key ielm-map (kbd "C-c C-z") 'conf-lisp/repl-switch-back)

(setq load-prefer-newer t)

(defconst conf-lisp/elispy-mode-hooks
  '(emacs-lisp-mode-hook ielm-mode-hook)
  "Major modes relating to elisp.")

(defconst conf-lisp/lispy-mode-hooks
  (append conf-lisp/elispy-mode-hooks
          '(lisp-mode-hook inferior-lisp-mode-hook lisp-interaction-mode-hook))
  "All lispy major modes.")

(defun conf-lisp/lisp-setup ()
  "Enable features useful in any Lisp mode."
  (rainbow-delimiters-mode t)
  (enable-paredit-mode)
  (aggressive-indent-mode)
  (turn-on-eldoc-mode)
  (add-hook 'after-save-hook #'check-parens nil t))

(defun conf-lisp/emacs-lisp-setup ()
  "Enable features useful when working with elisp."
  (elisp-slime-nav-mode t))

(dolist (hook conf-lisp/lispy-mode-hooks)
  (add-hook hook 'conf-lisp/lisp-setup))

(dolist (hook conf-lisp/elispy-mode-hooks)
  (add-hook hook 'conf-lisp/emacs-lisp-setup))

(add-to-list 'auto-mode-alist '("\\.emacs-project\\'" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("archive-contents\\'" . emacs-lisp-mode))

(global-set-key (kbd "C-h K") 'find-function-on-key)

(eldoc-add-command
 'paredit-backward-delete
 'paredit-close-round)

(provide 'conf-lisp)
