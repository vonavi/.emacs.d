;;; server.el

(use-package server
  :config (or (server-running-p) (server-mode +1)))

(provide 'config/server)
;;; server.el ends here
