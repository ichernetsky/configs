(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))

(setq el-get-sources
      '((:name show-wspace
               :after (add-hook 'c-mode-hook
                                (lambda ()
                                  (show-ws-highlight-tabs)
                                  (show-ws-highlight-hard-spaces)
                                  (show-ws-highlight-trailing-whitespace))))))

(setq dim-packages
      (append
       ;; list of packages we use straight from official recipes
       '(markdown-mode cmake-mode)

       (mapcar 'el-get-as-symbol (mapcar 'el-get-source-name el-get-sources))))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(el-get 'sync dim-packages)

;; do not make backup files
(setq make-backup-files nil)
(auto-save-mode nil)
;; remove the splash screen at startup
(setq inhibit-splash-screen t)
;; make emacs ask about missing newline
(setq require-final-newline 'ask)
(setq-default indent-tabs-mode nil)

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; disable unnessary clutter
(menu-bar-mode nil)
(tool-bar-mode nil)
(scroll-bar-mode nil)

(global-font-lock-mode t)
(ido-mode t)
(show-paren-mode t)
(column-number-mode t)

(global-visual-line-mode t)
(iswitchb-mode t)

(setq c-basic-offset 4)
(setq c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
                        (other . "k&r")))

(setq user-full-name "Ivan Chernetsky")
(setq gnus-select-method '(nntp "news.sunsite.dk"))

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "chromium")
