;; tell distel to default to that node
(setq-default erl-nodename-cache
              (make-symbol
               (concat
                "emacs@127.0.0.1")))

(provide 'conf-erlang)
