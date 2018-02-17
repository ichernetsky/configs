;; prevent annoying hang-on-compile
(setq-default inferior-erlang-prompt-timeout t)

;; default node name to emacs@localhost
(setq-default inferior-erlang-machine-options '("-name" "emacs@127.0.0.1"))

;; tell distel to default to that node
(setq-default erl-nodename-cache
              (make-symbol
               (concat
                "emacs@127.0.0.1")))

(provide 'conf-erlang)
