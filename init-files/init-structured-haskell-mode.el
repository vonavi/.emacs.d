;;; init-structured-haskell-mode.el ---

;; Copyright (C) 2015  Vladimir Ivanov

;; Author: Vladimir Ivanov <ivvl82@gmail.com>
;; Keywords:

;;; Commentary:

;;

;;; Code:

(require 'shm)
(add-hook 'haskell-mode-hook 'structured-haskell-mode)

;; The following are apparently pretty good for solarized-light
(set-face-background 'shm-current-face "#eee8d5")
(set-face-background 'shm-quarantine-face "lemonchiffon")

;;; init-structured-haskell-mode.el ends here
