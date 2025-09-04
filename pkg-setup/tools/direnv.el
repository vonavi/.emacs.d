;;; tools/direnv.el

;;
;;; Support for `direnv' that operates buffer-locally

(use-package envrc
  :delight
  :ensure-system-package direnv
  :hook (after-init . envrc-global-mode)
  :config
  ;; Ensure babel's execution environment matches the host buffer's
  (advice-add #'org-babel-execute-src-block
              :around #'envrc-propagate-environment))

(provide 'tools/direnv)
;;; tools/direnv.el ends here
