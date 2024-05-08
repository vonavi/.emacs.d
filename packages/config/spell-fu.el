;;; spell-fu.el

(use-package spell-fu
  :ensure-system-package aspell
  :config
  (spell-fu-global-mode +1)
  :hook
  ;; Use multiple languages in the same buffer
  (spell-fu-mode . (lambda ()
                     (spell-fu-dictionary-add
                      (spell-fu-get-ispell-dictionary "ru")))))

(provide 'config/spell-fu)
;;; spell-fu.el ends here
