;;; display-fill-column-indicator.el

(use-package display-fill-column-indicator
  :init
  (setq-default display-fill-column-indicator-column 80)
  :hook
  (prog-mode . (lambda () (display-fill-column-indicator-mode +1))))

(provide 'config/display-fill-column-indicator)
;;; display-fill-column-indicator.el ends here
