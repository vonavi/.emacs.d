;;; lang/python.el

(use-package python
  :ensure nil
  :ensure-system-package (pylsp . python3-pylsp)
  :after (eglot treesit)
  :custom
  ;; Silence warnings about Python indentation
  (python-indent-guess-indent-offset-verbose nil)
  :init
  ;; Install the tree-sitter grammar library
  (add-to-list 'treesit-language-source-alist
               '(python "https://github.com/tree-sitter/tree-sitter-python"))
  ;; Tell Emacs to prefer the tree-sitter mode
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
  :hook
  (python-ts-mode . eglot-ensure)       ; start the LSP server
  (python-ts-mode . (lambda () (setq-local tab-width 4))))

(provide 'lang/python)
;;; lang/python.el ends here
