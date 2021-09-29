;;; init-ivy.el ---

;; Copyright (C) 2021  Vladimir Ivanov

;; Author: Vladimir Ivanov <ivvl82@gmail.com>
;; Keywords:

;;; Commentary:

;;

;;; Code:

;; Enable generic Ivy completion
(ivy-mode 1)

;; Make the prompt line selectable like a candidate
(setq ivy-use-selectable-prompt t)

;; Enable switching to the "special" buffers
(setq ivy-use-virtual-buffers t)

;; Create and delete a view
(global-set-key (kbd "C-c v") 'ivy-push-view)
(global-set-key (kbd "C-c V") 'ivy-pop-view)

;;; init-ivy.el ends here
