;;; nlinum.el

(use-package nlinum
  :hook (prog-mode . (lambda () (nlinum-mode +1))))

(provide 'config/nlinum)
;;; nlinum.el ends here
