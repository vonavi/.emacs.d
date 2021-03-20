;;; early-init.el ---

;; Copyright (C) 2021  Vladimir Ivanov

;; Author: Vladimir Ivanov <ivvl82@gmail.com>
;; Keywords:

;;; Commentary:

;;

;;; Code:

;; In Emacs 27+, package initialization occurs before `user-init-file'
;; is loaded, but after `early-init-file'.
(setq package-enable-at-startup nil)

;;; early-init.el ends here
