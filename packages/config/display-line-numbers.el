;;; display-line-numbers.el

(use-package display-line-numbers
  ;; Avoid the annoying shifting of line number width
  :init (setq-default display-line-numbers-width 4)
  :hook (prog-mode . (lambda () (display-line-numbers-mode +1))))

(provide 'config/display-line-numbers)
;;; display-line-numbers.el ends here
