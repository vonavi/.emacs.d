;;; init-magit.el ---

;; Copyright (C) 2014  Vladimir Ivanov

;; Author: Vladimir Ivanov <ivvl82@gmail.com>
;; Keywords:

;;; Commentary:

;;

;;; Code:

;; Magit's status buffer
(global-set-key (kbd "C-x m") 'magit-status)

(add-hook 'magit-mode-hook (lambda () (setq truncate-lines nil)))

;; Show fine (word-granularity) differences within diff hunks
(setq magit-diff-refine-hunk 'all)

;; Do not show the diff before writing a commit message
(setq magit-commit-show-diff nil)

;;; init-magit.el ends here
