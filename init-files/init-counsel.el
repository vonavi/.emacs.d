;;; el-get-counsel.el ---

;; Copyright (C) 2021  Vladimir Ivanov

;; Author: Vladimir Ivanov <ivvl82@gmail.com>
;; Keywords:

;;; Commentary:

;;

;;; Code:

;; Enable an isearch-like swiper
(global-set-key [remap isearch-forward] #'swiper-isearch)

;; Remap built-in Emacs functions that have Counsel replacements
(counsel-mode 1)

;;; el-get-counsel.el ends here
