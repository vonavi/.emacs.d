;;; magit.el ---

;; Copyright (C) 2024  Vladimir Ivanov

;; Author: Vladimir Ivanov <ivvl82@gmail.com>
;; Keywords:

;;; Commentary:

;;

;;; Code:

(use-package magit
  :init
  ;; Show fine (word-granularity) differences within diff hunks
  (setq magit-diff-refine-hunk 'all)

  ;; Magit's status buffer
  :bind ("C-x m" . magit-status)

  ;; Fold long lines
  :hook (magit-mode . (lambda () (toggle-truncate-lines -1))))

(provide 'config/magit)
;;; magit.el ends here
