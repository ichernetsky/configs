(when (eq window-system 'ns)
  (setenv "PATH" (format "%s:%s" "/usr/local/bin" (getenv "PATH")))
  (setq exec-path (append '("/usr/local/bin") exec-path))

  (when (locate-file "gnutls-cli" exec-path)
    (setq tls-program
          '("gnutls-cli --x509cafile /etc/ssl/certs/ca-certificates.crt -p %p %h --no-ca-verification --no-ocsp"
            "gnutls-cli --x509cafile /etc/ssl/certs/ca-certificates.crt -p %p %h --protocols ssl3 --no-ca-verification --no-ocsp"
            "openssl s_client -connect %h:%p -CAfile /etc/ssl/certs/ca-certificates.crt -no_ssl2 -ign_eof"))))

(provide 'conf-mac)
