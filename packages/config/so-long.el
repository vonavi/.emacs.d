;;; so-long.el

(use-package so-long
  :ensure nil
  ;; Avoid performance issues in files with very long lines
  :config (global-so-long-mode +1))

(provide 'config/so-long)
;;; so-long.el ends here
