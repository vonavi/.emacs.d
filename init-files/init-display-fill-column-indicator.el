;;; init-display-fill-column-indicator.el ---

;; Copyright (C) 2021  Vladimir Ivanov

;; Author: Vladimir Ivanov <ivvl82@gmail.com>
;; Keywords:

;;; Commentary:

;;

;;; Code:

(setq-default display-fill-column-indicator-column 80)
(add-hook 'prog-mode-hook (lambda () (display-fill-column-indicator-mode 1)))

;;; init-display-fill-column-indicator.el ends here
