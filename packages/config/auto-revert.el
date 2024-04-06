;;; auto-revert.el

(use-package autorevert
  :ensure nil
  :delight auto-revert-mode
  ;; Keeping buffers automatically up-to-date
  :config (global-auto-revert-mode +1))

(provide 'config/auto-revert)
;;; auto-revert.el ends here
