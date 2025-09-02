;;; desktop.el

(use-package desktop
  :ensure nil
  :custom
  (desktop-path
   (list (expand-file-name "desktop/" user-emacs-directory)))
  (desktop-load-locked-desktop t)    ; load the desktop without asking
  :config
  (desktop-save-mode +1))

(provide 'config/desktop)
;;; desktop.el ends here
