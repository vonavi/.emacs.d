;;; lang/python.el

(use-package python
  :ensure nil
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
  :config
  ;; Configure Python LSP server
  (setq-default
   eglot-workspace-configuration
   (plist-put eglot-workspace-configuration
              :pylsp
              '(:plugins (:pylsp_mypy (:enabled t)))))
  :hook
  (python-ts-mode . eglot-ensure)       ; start Python LSP server
  (python-ts-mode . (lambda () (setq-local tab-width 4))))

(provide 'lang/python)
;;; lang/python.el ends here
