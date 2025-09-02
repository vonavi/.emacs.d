;;; undo-tree.el

(use-package undo-tree
  :delight
  :custom
  ;; Save history to a file
  (undo-tree-auto-save-history t)
  ;; Back up all undo-history files into one directory
  (undo-tree-history-directory-alist
   `(("." . ,(expand-file-name "undo/" user-emacs-directory))))
  :config
  (global-undo-tree-mode +1))

(provide 'config/undo-tree)
;;; undo-tree.el ends here
