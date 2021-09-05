;;; init-dune-format.el ---

;; Copyright (C) 2021  Vladimir Ivanov

;; Author: Vladimir Ivanov <ivvl82@gmail.com>
;; Keywords:

;;; Commentary:

;;

;;; Code:

;; Enable `dune-format-on-save-mode' in dune-mode buffers
(add-hook 'dune-mode-hook 'dune-format-on-save-mode)

;;; init-dune-format.el ends here
