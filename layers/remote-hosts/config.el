(with-eval-after-load 'tramp-sh
  ;; ;; ;; on all hosts, sudo method uses this proxy
  (add-to-list 'tramp-default-proxies-alist
               '(nil "\\`root\\'" "/ssh:%h:"))

  (add-to-list 'tramp-actions-before-shell
               '(tramp-blank-line-regexp tramp-enter-synchronous-response))

  (add-to-list 'tramp-actions-before-shell
               '(tramp-passcode-prompt tramp-two-factor-send-passcode))

  ;; ;; uses ~/.ssh/config
  (setq tramp-use-ssh-controlmaster-options nil)

  ;; removes "Passcode" from default value so we can use tramp-two-factor-send-passcode
  (setq tramp-password-prompt-regexp
        (concat
         "^.*"
         (regexp-opt
          '("Passphrase" "passphrase"
            "Password" "password") t)
         ".*:?\0? *")))
