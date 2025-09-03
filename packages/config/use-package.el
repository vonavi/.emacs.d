;;; use-package.el

;; Always install packages if they are not installed
(use-package use-package
  :ensure nil
  :custom (use-package-always-ensure t))

;; Allow to ensure system binaries exist alongside your package declarations
(use-package system-packages
  ;; Bypass prompts asking the user to confirm package upgrades
  :custom (system-packages-noconfirm t))

;; Customize the mode names displayed in the mode line
(use-package delight)

(provide 'config/use-package)
;;; use-package.el ends here
