(require 'cc-mode)
(require 'company)

(define-key c++-mode-map [(tab)] 'company-complete)

(provide 'conf-cc)
