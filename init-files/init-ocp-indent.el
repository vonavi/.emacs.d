;;; init-ocp-indent.el ---

;; Copyright (C) 2021  Vladimir Ivanov

;; Author: Vladimir Ivanov <ivvl82@gmail.com>
;; Keywords:

;;; Commentary:

;;

;;; Code:

;; Apply `ocp-indent' on saving a buffer
(add-hook 'tuareg-mode-hook
          (lambda () (add-hook 'before-save-hook #'ocp-indent-buffer nil t)))
(add-hook 'caml-mode-hook
          (lambda () (add-hook 'before-save-hook #'ocp-indent-buffer nil t)))

;;; init-ocp-indent.el ends here
