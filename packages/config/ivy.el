;;; ivy.el ---

;; Copyright (C) 2023  Vladimir Ivanov

;; Author: Vladimir Ivanov <ivvl82@gmail.com>
;; Keywords:

;;; Commentary:

;;

;;; Code:

(use-package ivy
  :bind
  ;; Create and delete a view
  (("C-c v" . ivy-push-view)
   ("C-c V" . ivy-pop-view))
  :init
  (setq ivy-use-selectable-prompt t ; make the prompt line selectable like a candidate
        ivy-use-virtual-buffers t) ; enable switching to the "special" buffers
  :config
  (ivy-mode +1))

(provide 'config/ivy)

;;; ivy.el ends here
