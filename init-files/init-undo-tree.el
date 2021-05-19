;;; init-undo-tree.el ---

;; Copyright (C) 2016  Vladimir Ivanov

;; Author: Vladimir Ivanov <ivvl82@gmail.com>
;; Keywords:

;;; Commentary:

;;

;;; Code:

(global-undo-tree-mode 1)

(setq undo-tree-auto-save-history t)
;; Back up all undo-history files into one directory
(setq undo-tree-history-directory-alist
      `(("." . ,(concat user-emacs-directory "undo"))))

;;; init-undo-tree.el ends here
