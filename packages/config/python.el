;;; python.el

(use-package python
  :ensure nil
  :ensure-system-package (pylsp . python3-pylsp)
  :after (eglot treesit)
  :init
  ;; Install the tree-sitter grammar library
  (add-to-list 'treesit-language-source-alist
               '(python "https://github.com/tree-sitter/tree-sitter-python"))
  ;; Tell Emacs to prefer the tree-sitter mode
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))

  :hook
  ((python-ts-mode . eglot-ensure)      ; start the LSP server
   (python-ts-mode . (lambda () (setq tab-width 4)))))

(provide 'config/python)
;;; python.el ends here
