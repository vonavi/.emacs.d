;;; visual-fill-column.el

(use-package simple
  :ensure nil
  :delight visual-line-mode
  :custom (visual-line-fringe-indicators '(nil right-curly-arrow))
  :hook (text-mode . turn-on-visual-line-mode))

(use-package visual-fill-column
  :custom (visual-fill-column-fringes-outside-margins nil)
  :hook (visual-line-mode . turn-on-visual-fill-column-mode))

(provide 'config/visual-fill-column)
;;; visual-fill-column.el ends here
