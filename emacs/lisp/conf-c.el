(require 'cc-mode)
(require 'company)

(setq c-basic-offset 4)
(setq c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
                        (other . "k&r")))

(define-key c-mode-map [(tab)] 'company-complete)

(provide 'conf-c)
