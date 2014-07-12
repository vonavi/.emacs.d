;;; init-window-margin.el ---

;; Copyright (C) 2014 Vladimir Ivanov

;; Author: Vladimir Ivanov <ivvl82@gmail.com>

;;; Commentary:

;;; Code:

;; Automatic margins for visual-line-mode wrapping
(setq window-margin-width t)
(add-hook 'text-mode-hook 'turn-on-window-margin-mode)

;;; init-window-margin.el ends here
