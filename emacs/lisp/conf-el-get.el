(add-to-list 'load-path
             (expand-file-name "el-get/el-get" user-emacs-directory))

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let ((el-get-git-install-url "git@github.com:dimitri/el-get.git"))
      (goto-char (point-max))
      (eval-print-last-sexp))))

(add-to-list 'el-get-recipe-path
             (expand-file-name "el-get-user/recipes" user-emacs-directory))
(setq el-get-user-package-directory
      (expand-file-name "lisp" user-emacs-directory))

(setq el-get-sources
      '((:name distel
         :depends nil
         :info nil
         :build/darwin (list (list "make" (format "EMACS=%s" el-get-emacs) "clean" "base")))))

(el-get 'sync '(color-theme-tangotango
                diminish beacon which-key volatile-highlights
                anzu undo-tree dtrt-indent
                elisp-slime-nav auto-compile
                highlight-chars projectile
                helm helm-descbinds helm-ag helm-gtags
                helm-projectile
                gitconfig magit
                cmake-mode puppet-mode dockerfile-mode
                yaml-mode markdown-mode go-mode lua-mode
                protobuf-mode web-mode distel
                paredit rainbow-delimiters hl-sexp
                company-mode clojure-mode cider))

(provide 'conf-el-get)
