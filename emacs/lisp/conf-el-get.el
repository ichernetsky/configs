(add-to-list 'load-path
             (expand-file-name "el-get/el-get" user-emacs-directory))

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))

(add-to-list 'el-get-recipe-path
             (expand-file-name "el-get-user/recipes" user-emacs-directory))
(setq el-get-user-package-directory
      (expand-file-name "lisp" user-emacs-directory))

(provide 'conf-el-get)
