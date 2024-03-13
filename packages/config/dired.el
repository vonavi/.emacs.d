;;; dired.el

(use-package dired
  :ensure nil
  :init
  ;; Kill the current buffer when selecting a new directory
  (setq dired-kill-when-opening-new-dired-buffer t))

(provide 'config/dired)
;;; dired.el ends here
