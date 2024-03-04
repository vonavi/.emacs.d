;;; nlinum.el ---

;; Copyright (C) 2024  Vladimir Ivanov

;; Author: Vladimir Ivanov <ivvl82@gmail.com>
;; Keywords:

;;; Commentary:

;;

;;; Code:

(use-package nlinum
  :hook (prog-mode . (lambda () (nlinum-mode +1))))

(provide 'config/nlinum)
;;; nlinum.el ends here
