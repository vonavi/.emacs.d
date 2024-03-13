;;; smart-hungry-delete.el

(use-package smart-hungry-delete
  :config (smart-hungry-delete-add-default-hooks)
  :bind
  (([remap delete-backward-char] . smart-hungry-delete-backward-char)
   ([remap backward-delete-char-untabify] . smart-hungry-delete-backward-char)
   ([remap delete-forward-char] . smart-hungry-delete-forward-char)))

(provide 'config/smart-hungry-delete)
;;; smart-hungry-delete.el ends here
