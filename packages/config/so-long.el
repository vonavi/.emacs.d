;;; so-long.el ---                                   -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Vladimir Ivanov

;; Author: Vladimir Ivanov <ivvl82@gmail.com>
;; Keywords:

;;; Commentary:

;;

;;; Code:

(use-package so-long
  ;; Avoid performance issues in files with very long lines
  :config (global-so-long-mode +1))

(provide 'config/so-long)
;;; so-long.el ends here
