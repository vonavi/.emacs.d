;;; tools/lsp.el

(use-package eglot
  :ensure nil
  ;; Shut down server after killing last managed buffer
  :custom (eglot-autoshutdown t))

(provide 'tools/lsp)
;;; tools/lsp.el ends here
