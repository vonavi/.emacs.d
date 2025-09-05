;;; tools/make.el

;;
;;; Commands for conveniently running makefile targets

(use-package makefile-executor
  :hook (makefile-mode . makefile-executor-mode))

(provide 'tools/make)
;;; tools/make.el ends here
