;;; treesit.el

(use-package treesit
  :ensure nil
  :init
  ;; M-x `treesit-install-language-grammar' to install language grammar
  (setq treesit-language-source-alist
        '((python "https://github.com/tree-sitter/tree-sitter-python")))
  ;; Tell Emacs to prefer the tree-sitter mode
  (setq major-mode-remap-alist
        '((python-mode . python-ts-mode))))

(provide 'config/treesit)
;;; treesit.el ends here
