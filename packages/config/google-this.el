;;; google-this.el

(use-package google-this
  :config (google-this-mode +1)
  ;; Reassign keybinding to personal preferences
  :bind-keymap ("C-x g" . google-this-mode-submap))

(provide 'config/google-this)
;;; google-this.el ends here
