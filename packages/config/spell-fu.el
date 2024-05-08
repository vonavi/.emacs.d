;;; spell-fu.el

(use-package spell-fu
  :ensure-system-package aspell
  :after org
  :hook
  ((spell-fu-mode . (lambda ()
                      ;; Use multiple languages in the same buffer
                      (spell-fu-dictionary-add
                       (spell-fu-get-ispell-dictionary "ru"))))
   ((prog-mode text-mode) . (lambda ()
                              (spell-fu-mode +1)))
   (org-mode . (lambda ()
                 (setq spell-fu-faces-exclude
                       '(org-block
                         org-block-begin-line
                         org-block-end-line
                         org-code
                         org-meta-line
                         org-special-keyword))
                 (spell-fu-mode +1)))))

(provide 'config/spell-fu)
;;; spell-fu.el ends here
