;;; init-el-get.el ---

;; Copyright (C) 2015-2021  Vladimir Ivanov

;; Author: Vladimir Ivanov <ivvl82@gmail.com>
;; Keywords:

;;; Commentary:

;;

;;; Code:

(defvar my:el-get-packages
  '(auctex          ; integrated environment for *TeX*
    (avy            ; jump to things in Emacs tree-style
     avy-zap)       ; zap to char using `avy'
    dired+          ; extensions to Dired
    dockerfile-mode ; major mode for editing Docker's Dockerfiles
    (dune           ; integration with the dune build system
     merlin         ; mode for Merlin, an assistant for OCaml
     ocp-indent     ; automatic indentation with ocp-indent
     tuareg-mode)   ; an Emacs OCaml mode
    graphviz-dot-mode               ; mode for the dot-language used by graphviz
    magit       ; it's Magit! An Emacs mode for Git
    (org-mode   ; outline-based notes management and organizer
     org-ref) ; citations, cross-references and bibliographies in org-mode
    pomidor   ; simple and cool pomodoro timer
    protobuf-mode        ; major mode for editing protocol buffers
    rainbow-delimiters   ; highlight brackets according to their depth
    sdcv              ; interface for sdcv (StartDict console version)
    solarized-emacs)  ; the Solarized color theme
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
