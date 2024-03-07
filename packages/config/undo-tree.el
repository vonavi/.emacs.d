;;; undo-tree.el ---

;; Copyright (C) 2023  Vladimir Ivanov

;; Author: Vladimir Ivanov <ivvl82@gmail.com>
;; Keywords:

;;; Commentary:

;;

;;; Code:

(use-package undo-tree
  :init
  (setq undo-tree-auto-save-history t   ; save history to a file
        ;; Back up all undo-history files into one directory
        undo-tree-history-directory-alist
        `(("." . ,(concat user-emacs-directory "undo"))))
  :config (global-undo-tree-mode +1))

(provide 'config/undo-tree)
;;; undo-tree.el ends here
