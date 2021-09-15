;;; init-el-get.el ---

;; Copyright (C) 2015-2021  Vladimir Ivanov

;; Author: Vladimir Ivanov <ivvl82@gmail.com>
;; Keywords:

;;; Commentary:

;;

;;; Code:

(defvar my:builtin-packages
  '(auto-insert ; automatic mode-dependent insertion of text into new files
    display-fill-column-indicator ; interface for display-fill-column-indicator
    flyspell)                     ; on-the-fly spell checker
  "A list of built-in packages, initialization files for which
  are loaded at launch.")

(defvar my:el-get-packages
  '((adaptive-wrap   ; smart line-wrapping with wrap-prefix
     nlinum          ; show line numbers in the margin
     window-margin)  ; automatic margins for visual-line-mode wrapping
    auctex           ; integrated environment for *TeX*
    (avy             ; jump to things in Emacs tree-style
     avy-zap)        ; zap to char using `avy'
    (counsel         ; various completion functions using Ivy
     ivy             ; Incremental Vertical completYon
     ivy-prescient)  ; prescient.el + Ivy
    (dune            ; integration with the dune build system
     merlin          ; mode for Merlin, an assistant for OCaml
     proof-general ; a generic Emacs interface for interactive proof assistants
     tuareg-mode)  ; an Emacs OCaml mode
    (hungry-delete ; enables hungry deletion in all modes
     undo-tree     ; treat undo history as a tree
     ws-butler) ; unobtrusively trim extraneous white-space *ONLY* in lines edited
    magit       ; it's Magit! An Emacs mode for Git
    org         ; outline-based notes management and organizer
    pomidor     ; simple and cool pomodoro timer
    sdcv        ; interface for sdcv (StartDict console version)
    solarized-emacs)                    ; the Solarized color theme
  "A list of packages to ensure are installed at launch.")

;; Load initialization files for built-in packages
(dolist (pkg my:builtin-packages)
  (let ((init-file (expand-file-name (concat "init-" (symbol-name pkg))
                                     el-get-user-package-directory)))
    (el-get-byte-compile-file (concat init-file ".el") byte-compile-warnings)
    (load init-file)))

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
