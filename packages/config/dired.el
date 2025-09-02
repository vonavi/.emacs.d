;;; dired.el

(use-package dired
  :ensure nil
  ;; Kill the current buffer when selecting a new directory
  :custom (dired-kill-when-opening-new-dired-buffer t))

(provide 'config/dired)
;;; dired.el ends here
