;;; vertico.el ---

;; Copyright (C) 2024  Vladimir Ivanov

;; Author: Vladimir Ivanov <ivvl82@gmail.com>
;; Keywords:

;;; Commentary:

;;

;;; Code:

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
  :init
  ;; Enable orderless completion
  (setq completion-styles '(orderless basic))
  ;; Set partial-completion for the file completion category only
  (setq completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(provide 'config/vertico)
;;; vertico.el ends here
