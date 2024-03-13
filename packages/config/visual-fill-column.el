;;; visual-fill-column.el

(use-package visual-fill-column
  :init
  (setq visual-line-fringe-indicators '(nil right-curly-arrow)
        visual-fill-column-fringes-outside-margins nil)
  :hook ((text-mode . turn-on-visual-line-mode)
         (visual-line-mode . turn-on-visual-fill-column-mode)))

(provide 'config/visual-fill-column)
;;; visual-fill-column.el ends here
