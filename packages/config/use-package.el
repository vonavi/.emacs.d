;;; use-package.el

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Always install packages if they are not installed
(use-package use-package-ensure
  :init (setq use-package-always-ensure t))

;; Allow to ensure system binaries exist alongside your package declarations
(use-package system-packages
  ;; Bypass prompts asking the user to confirm package upgrades
  :init (setq system-packages-noconfirm t))
(use-package use-package-ensure-system-package)

(provide 'config/use-package)
;;; use-package.el ends here
