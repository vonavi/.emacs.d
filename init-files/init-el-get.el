;;; init-el-get.el ---

;; Copyright (C) 2015, 2016  Vladimir Ivanov

;; Author: Vladimir Ivanov <ivvl82@gmail.com>
;; Keywords:

;;; Commentary:

;;

;;; Code:

(defvar my:builtin-packages
  '(auto-insert ; automatic mode-dependent insertion of text into new files
    flyspell)   ; on-the-fly spell checker
  "A list of built-in packages, initialization files for which
  are loaded at launch.")
(defvar my:el-get-packages
  '(ace-jump-mode           ; fast buffer switching extension to `avy'
    adaptive-wrap           ; smart line-wrapping with wrap-prefix
    auctex                  ; integrated environment for *TeX*
    essh    ; a set of commands that emulate for bash what ESS is to R
    fill-column-indicator       ; graphically indicate the fill column
    fullscreen                  ; full screen
    hungry-delete               ; hungry delete minor mode
    magit                       ; a Git porcelain inside Emacs
    nlinum                      ; show line numbers in the margin
    org     ; outline-based notes management and organizer
    pomidor ; simple and cool http://www.pomodorotechnique.com timer
    realgud ; a modular front-end for interacting with external debuggers
    sdcv    ; interface for sdcv (StartDict console version)
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

(el-get-cleanup my:el-get-packages)
(el-get 'sync my:el-get-packages)

;;; init-el-get.el ends here
