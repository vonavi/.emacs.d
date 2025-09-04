;;; files.el

(use-package files
  :ensure nil
  :custom
  ;; Automatically kill running processes on exit
  (confirm-kill-processes nil)
  ;; Don't litter my fs tree
  (backup-directory-alist
   `(("." . ,(expand-file-name "backup/" user-emacs-directory))))
  (backup-by-copying t)             ; don't clobber symlinks
  (delete-old-versions t)           ; delete intermediate backup files
  (kept-old-versions 0)             ; don't keep first backups
  (kept-new-versions 4)             ; keep 4 last backups
  (version-control t))              ; use versioned backups

(provide 'config/files)
;;; files.el ends here
