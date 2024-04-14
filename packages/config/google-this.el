;;; google-this.el

(use-package google-this
  :delight
  ;; Reassign keybinding to personal preferences
  :init (setq google-this-keybind (kbd "C-x g"))
  :config (google-this-mode +1))

(provide 'config/google-this)
;;; google-this.el ends here
