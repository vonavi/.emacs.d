;;; avy.el

(use-package avy
  :after org
  :init (unbind-key "C-'" org-mode-map)
  :bind (("C-;" . avy-goto-char)
         ("C-'" . avy-goto-char-2)))

(use-package avy-zap
  :bind (("M-z" . avy-zap-to-char-dwim)
         ("M-Z" . avy-zap-up-to-char-dwim)))

(provide 'config/avy)
;;; avy.el ends here
