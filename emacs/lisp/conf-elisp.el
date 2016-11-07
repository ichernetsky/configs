(require 'eldoc)
(require 'ielm)

(defvar conf-elisp/repl-original-buffer nil
  "Buffer from which we jumped to this REPL.")
(make-variable-buffer-local 'conf-elisp/repl-original-buffer)

(defvar conf-elisp/repl-switch-function 'switch-to-buffer-other-window)

(defun conf-elisp/switch-to-ielm ()
  (interactive)
  (let ((orig-buffer (current-buffer)))
    (if (get-buffer "*ielm*")
        (funcall conf-elisp/repl-switch-function "*ielm*")
      (ielm))
    (setq conf-elisp/repl-original-buffer orig-buffer)))

(defun conf-elisp/repl-switch-back ()
  "Switch back to the buffer from which we reached this REPL."
  (interactive)
  (if conf-elisp/repl-original-buffer
      (funcall conf-elisp/repl-switch-function conf-elisp/repl-original-buffer)
    (error "No original buffer.")))

(define-key emacs-lisp-mode-map (kbd "C-c C-z") 'conf-elisp/switch-to-ielm)
(define-key ielm-map (kbd "C-c C-z") 'conf-elisp/repl-switch-back)

(defun conf-elisp/hook-fn ()
  "Enable features useful when working with elisp."
  (rainbow-delimiters-mode t)
  (enable-paredit-mode)
  (turn-on-eldoc-mode)
  (add-hook 'after-save-hook #'check-parens nil t)
  (elisp-slime-nav-mode t))

(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'conf-elisp/hook-fn))

(add-to-list 'auto-mode-alist '("\\.emacs-project\\'" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("archive-contents\\'" . emacs-lisp-mode))

(eldoc-add-command
 'paredit-backward-delete
 'paredit-close-round)

(provide 'conf-elisp)
