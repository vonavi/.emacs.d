;;; tools/direnv.el

;;
;;; Support for `direnv' that operates buffer-locally

(use-package envrc
  :delight
  :ensure-system-package direnv
  :hook (after-init . envrc-global-mode)
  :config
  ;; Ensure Org Babel's execution environment matches the host buffer's
  (with-eval-after-load 'ox
    (advice-add #'org-export-as
                :around #'envrc-propagate-environment)))

(provide 'tools/direnv)
;;; tools/direnv.el ends here
