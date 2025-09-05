;;; editor/file-templates.el

(use-package autoinsert
  :ensure nil
  :ensure-system-package git
  :custom
  (auto-insert-query nil) ; don't prompt before insertion
  (auto-insert 'other)    ; insert if possible, but mark as unmodified
  ;; User information to be auto-inserted
  (user-full-name
   (string-trim-right (shell-command-to-string "git config user.name")))
  (user-mail-address
   (string-trim-right (shell-command-to-string "git config user.email")))
  :config
  (auto-insert-mode +1))

(provide 'editor/file-templates)
;;; editor/file-templates.el ends here
