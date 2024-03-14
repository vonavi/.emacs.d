;;; init-el-get.el ---

;; Copyright (C) 2015-2021  Vladimir Ivanov

;; Author: Vladimir Ivanov <ivvl82@gmail.com>
;; Keywords:

;;; Commentary:

;;

;;; Code:

(defvar my:el-get-packages
  '(dune                     ; integration with the dune build system
    merlin                   ; mode for Merlin, an assistant for OCaml
    ocp-indent               ; automatic indentation with ocp-indent
    tuareg-mode)             ; an Emacs OCaml mode
  "A list of packages to ensure are installed at launch.")

;; Clean up and init packages installed by El-Get
(let* ((flatten (lambda (l)
                  (if (listp l)
                      (apply 'append
                             (mapcar (lambda (p) (funcall flatten p)) l))
                    (list l))))
       (packages (funcall flatten my:el-get-packages)))
  (el-get-cleanup packages)
  (el-get 'sync packages))

;;; init-el-get.el ends here
