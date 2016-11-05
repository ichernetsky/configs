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

;; idomenu
;; macrostep web-mode
;; flex-mode

(el-get 'sync '(color-theme-tangotango
                auto-compile
                highlight-chars hl-sexp rainbow-delimiters
                smex paredit magit gitconfig cider
                elisp-slime-nav clojure-mode company-mode distel
                cmake-mode puppet-mode dockerfile-mode
                yaml-mode markdown-mode go-mode lua-mode
                csv-mode protobuf-mode))

(provide 'conf-el-get)
