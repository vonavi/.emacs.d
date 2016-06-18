;;; init-fill-column-indicator.el ---

;; Copyright (C) 2016  Vladimir Ivanov

;; Author: Vladimir Ivanov <ivvl82@gmail.com>
;; Keywords:

;;; Commentary:

;;

;;; Code:

(add-hook 'prog-mode-hook 'turn-on-fci-mode)

(setq fci-rule-column 80
      fci-rule-width 2)

;;; init-fill-column-indicator.el ends here
