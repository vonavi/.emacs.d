;;; lang/cc.el

(use-package cc-mode
  :ensure nil
  :ensure-system-package clangd
  :after (eglot treesit)
  :custom
  (c-ts-mode-indent-style 'k&r)         ; the placement of braces
  :init
  ;; Install the tree-sitter grammar library
  (add-to-list 'treesit-language-source-alist
               '(c "https://github.com/tree-sitter/tree-sitter-c"))
  (add-to-list 'treesit-language-source-alist
               '(cpp "https://github.com/tree-sitter/tree-sitter-cpp"))
  ;; Tell Emacs to prefer the tree-sitter mode
  (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c-or-c++-mode . c-or-c++-ts-mode))
  :hook
  ;; Start the LSP server
  ((c-ts-mode c++-ts-mode c-or-c++-ts-mode) . eglot-ensure))

(provide 'lang/cc)
;;; lang/cc.el ends here
