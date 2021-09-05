;;; init-el-get.el ---

;; Copyright (C) 2015-2021  Vladimir Ivanov

;; Author: Vladimir Ivanov <ivvl82@gmail.com>
;; Keywords:

;;; Commentary:

;;

;;; Code:

(defvar my:builtin-packages
  '(auto-insert      ; automatic mode-dependent insertion of text into new files
    display-fill-column-indicator  ; interface for display-fill-column-indicator
    flyspell)                      ; on-the-fly spell checker
  "A list of built-in packages, initialization files for which
  are loaded at launch.")
(defvar my:el-get-packages
  '(ace-jump-mode           ; fast buffer switching extension to `avy'
    adaptive-wrap           ; smart line-wrapping with wrap-prefix
    auctex                  ; integrated environment for *TeX*
    (counsel                ; various completion functions using Ivy
     ivy                    ; Incremental Vertical completYon
     ivy-prescient)         ; prescient.el + Ivy
    essh    ; a set of commands that emulate for bash what ESS is to R
    fullscreen          ; full screen
    hungry-delete       ; hungry delete minor mode
    magit               ; a Git porcelain inside Emacs
    nlinum              ; show line numbers in the margin
    org                 ; outline-based notes management and organizer
    pomidor   ; simple and cool http://www.pomodorotechnique.com timer
    sdcv      ; interface for sdcv (StartDict console version)
    solarized-emacs  ; Solarized for Emacs
    undo-tree        ; treat undo history as a tree
    window-margin    ; automatic margins for visual-line-mode wrapping
    ws-butler)       ; unobtrusively remove trailing whitespace
  "A list of packages to ensure are installed at launch.")

;; Load initialization files for built-in packages
(let ((init-files (mapcar (lambda (pkg)
                            (concat (file-name-as-directory
                                     el-get-user-package-directory)
                                    "init-" (symbol-name pkg) ".el"))
                          my:builtin-packages)))
  (dolist (file init-files)
    (load-file (expand-file-name file el-get-user-package-directory))))

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
