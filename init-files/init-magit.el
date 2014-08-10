;;; init-magit.el ---

;; Copyright (C) 2014  Vladimir Ivanov

;; Author: Vladimir Ivanov <ivvl82@gmail.com>
;; Keywords:

;;; Commentary:

;;

;;; Code:

;; Magit's status buffer
(global-set-key (kbd "C-x m") 'magit-status)

;; Show fine (word-granularity) differences within diff hunks
(setq magit-diff-refine-hunk 'all)

;;; init-magit.el ends here
