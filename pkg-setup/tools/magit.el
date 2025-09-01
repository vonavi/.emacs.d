;;; tools/magit.el

(use-package magit
  :ensure-system-package git
  :custom
  ;; Show fine (word-granularity) differences within diff hunks
  (magit-diff-refine-hunk 'all)
  ;; Open Magit full-frame then restore windows on quit
  (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (magit-bury-buffer-function #'magit-restore-window-configuration)
  :bind
  ;; Magit's status buffer
  ("C-x m" . magit-status)
  :hook
  ;; Fold long lines
  (magit-mode . (lambda () (toggle-truncate-lines -1))))

(provide 'tools/magit)
;;; tools/magit.el ends here
