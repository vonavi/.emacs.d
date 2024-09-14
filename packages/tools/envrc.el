;;; tools/envrc.el

(use-package envrc
  :delight
  :ensure-system-package direnv
  :hook (after-init . envrc-global-mode)
  :config
  ;; Ensure babel's execution environment matches the host buffer's
  (advice-add #'org-babel-execute-src-block
              :around #'envrc-propagate-environment))

(provide 'tools/envrc)
;;; tools/envrc.el ends here
