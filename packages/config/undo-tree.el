;;; undo-tree.el

(use-package undo-tree
  :delight
  :init
  (setq undo-tree-auto-save-history t   ; save history to a file
        ;; Back up all undo-history files into one directory
        undo-tree-history-directory-alist
        `(("." . ,(expand-file-name "undo/" user-emacs-directory))))
  :config (global-undo-tree-mode +1))

(provide 'config/undo-tree)
;;; undo-tree.el ends here
