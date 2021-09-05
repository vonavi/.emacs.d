;;; init-merlin.el ---

;; Copyright (C) 2021  Vladimir Ivanov

;; Author: Vladimir Ivanov <ivvl82@gmail.com>
;; Keywords:

;;; Commentary:

;;

;;; Code:

;; Automatically start Merlin in OCaml buffers
(add-hook 'tuareg-mode-hook #'merlin-mode)
(add-hook 'caml-mode-hook #'merlin-mode)

;;; init-merlin.el ends here
