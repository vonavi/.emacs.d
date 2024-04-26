;;; files.el

(use-package files
  :ensure nil
  :init
  (setq backup-by-copying t             ; don't clobber symlinks
        backup-directory-alist          ; don't litter my fs tree
        `(("." . ,(expand-file-name "backup/" user-emacs-directory)))
        delete-old-versions t       ; delete intermediate backup files
        kept-old-versions 0         ; don't keep first backups
        kept-new-versions 4         ; keep 4 last backups
        version-control t))         ; use versioned backups

(provide 'config/files)
;;; files.el ends here
