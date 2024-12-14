;;; checkers/spell-fu.el

(use-package text-mode
  :ensure nil
  :init
  ;; Remove ispell from the default completion
  (setq text-mode-ispell-word-completion nil))

(use-package spell-fu
  :ensure-system-package aspell
  :hook
  ((spell-fu-mode . (lambda ()
                      ;; Use multiple languages in the same buffer
                      (spell-fu-dictionary-add
                       (spell-fu-get-ispell-dictionary "ru"))))
   ((prog-mode text-mode) . (lambda () (spell-fu-mode +1)))
   (org-mode . (lambda ()
                 (setq spell-fu-faces-exclude
                       '(org-block
                         org-block-begin-line
                         org-block-end-line
                         org-code
                         org-meta-line
                         org-special-keyword))
                 (spell-fu-mode +1)))))

(provide 'checkers/spell-fu)
;;; checkers/spell-fu.el ends here
