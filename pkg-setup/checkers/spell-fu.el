;;; checkers/spell-fu.el

(use-package text-mode
  :ensure nil
  ;; Remove ispell from the default completion
  :custom (text-mode-ispell-word-completion nil))

(use-package spell-fu
  :ensure-system-package aspell
  :hook
  (((prog-mode text-mode) . (lambda () (spell-fu-mode +1)))
   (spell-fu-mode . (lambda ()
                      ;; Use multiple languages in the same buffer
                      (spell-fu-dictionary-add
                       (spell-fu-get-ispell-dictionary "ru"))))
   (LaTeX-mode . (lambda ()
                   (setq spell-fu-faces-exclude
                         '(font-latex-math-face
                           font-latex-sedate-face
                           font-latex-warning-face
                           font-lock-constant-face
                           font-lock-function-name-face
                           font-lock-keyword-face
                           font-lock-type-face
                           font-lock-variable-name-face))))
   (org-mode . (lambda ()
                 (setq spell-fu-faces-exclude
                       '(org-block
                         org-block-begin-line
                         org-block-end-line
                         org-code
                         org-meta-line
                         org-special-keyword))))))

(provide 'checkers/spell-fu)
;;; checkers/spell-fu.el ends here
