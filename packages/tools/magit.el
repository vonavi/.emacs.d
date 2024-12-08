;;; tools/magit.el

(use-package magit
  :ensure-system-package git
  :init
  ;; Show fine (word-granularity) differences within diff hunks
  (setq magit-diff-refine-hunk 'all)
  ;; Open Magit full-frame then restore windows on quit
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1
        magit-bury-buffer-function #'magit-restore-window-configuration)

  ;; Magit's status buffer
  :bind ("C-x m" . magit-status)

  ;; Fold long lines
  :hook (magit-mode . (lambda () (toggle-truncate-lines -1))))

(provide 'tools/magit)
;;; tools/magit.el ends here
