(add-to-list 'load-path
             (expand-file-name "el-get/el-get" user-emacs-directory))

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
      (goto-char (point-max))
      (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path
             (expand-file-name "recipes" user-emacs-directory))
(setq-default el-get-user-package-directory
              (expand-file-name "lisp" user-emacs-directory))

(setq el-get-sources
      '((:name swiper
         :description "Gives you an overview as you search for a regex."
         :type github
         :pkgname "abo-abo/swiper"
         :build (("make" "compile"))
         :info nil)

        (:name distel
         :website "https://github.com/ichernetsky/distel"
         :description "Distributed Emacs Lisp for Erlang."
         :type github
         :pkgname "ichernetsky/distel"
         :depends (erlang-mode)
         :info nil
         :build `,(mapcar
                   (lambda (target)
                     (list "make" target (format "EMACS=%s" el-get-emacs)))
                   '("clean" "base"))
         :load-path ("elisp")
         :prepare (let ((distel-path (el-get-package-directory 'distel)))
                    (setq distel-ebin-directory (expand-file-name "ebin" distel-path))
                    (autoload 'distel-setup "distel")))))

(el-get 'sync '(queue auto-compile elisp-slime-nav diminish which-key
                ag undo-tree highlight-chars paredit
                rainbow-delimiters hl-sexp smex projectile
                gitconfig magit cmake-mode go-mode web-mode
                yaml-mode protobuf-mode markdown-mode dockerfile-mode
                distel scala-mode sbt-mode ensime clojure-mode cider
                neotree terraform-mode dracula-theme ggtags))

(provide 'conf-el-get)
