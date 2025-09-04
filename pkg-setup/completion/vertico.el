;;; completion/vertico.el

(use-package vertico
  :config (vertico-mode +1))

(use-package savehist
  :config (savehist-mode +1))

(use-package vertico-directory
  :ensure vertico
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package orderless
  :custom
  ;; Enable orderless completion
  (completion-styles '(orderless basic))
  ;; Use orderless completion exclusively
  (completion-category-defaults nil)
  ;; Set partial-completion for the file completion category only
  (completion-category-overrides '((file (styles partial-completion)))))

(provide 'completion/vertico)
;;; completion/vertico.el ends here
