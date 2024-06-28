;;; c-ts-mode.el

(use-package c-ts-mode
  :ensure nil
  :ensure-system-package clangd
  :after (eglot treesit)
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

  (setq c-ts-mode-indent-style 'k&r     ; the placement of braces
        c-ts-mode-indent-offset 4)      ; the size of an indent

  ;; Start the LSP server
  :hook ((c-ts-mode c++-ts-mode c-or-c++-ts-mode) . eglot-ensure))

(provide 'config/c-ts-mode)
;;; c-ts-mode.el ends here
