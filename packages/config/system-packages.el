;;; system-packages.el

(use-package system-packages
  ;; Bypass prompts asking the user to confirm package upgrades
  :init (setq system-packages-noconfirm t))

(provide 'config/system-packages)
;;; system-packages.el ends here
