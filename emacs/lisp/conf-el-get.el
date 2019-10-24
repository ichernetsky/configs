(setq el-get-sources
      '((:name swiper
         :description "Gives you an overview as you search for a regex."
         :type github
         :pkgname "abo-abo/swiper"
         :build (("make" "compile"))
         :info nil)

        (:name distel
         :website "https://github.com/massemanet/distel"
         :description "Distributed Emacs Lisp for Erlang."
         :type github
         :pkgname "massemanet/distel"
         ;; :depends (erlang)
         :info nil
         :build `,(cons
                   (list "git" "checkout" "23a7ca91675052c781729addb04ca04229b02245")
                   (mapcar
                    (lambda (target)
                      (list "make" target (format "emacs=%s" el-get-emacs)))
                    '("clean" "base")))
         :load-path ("elisp")
         :prepare (let ((distel-path (el-get-package-directory 'distel)))
                    (setq distel-ebin-directory (expand-file-name "ebin" distel-path))
                    (autoload 'distel-setup "distel")))))

(el-get 'sync '(swiper distel))

(provide 'conf-el-get)
