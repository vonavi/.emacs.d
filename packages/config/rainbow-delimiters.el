;;; rainbow-delimiters.el

(use-package rainbow-delimiters
  :hook (prog-mode . (lambda () (rainbow-delimiters-mode +1))))

(provide 'config/rainbow-delimiters)
;;; rainbow-delimiters.el ends here
