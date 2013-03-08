;;; init-latex.el ---

;; Copyright (C) 2013 Vladimir S. Ivanov

;; Author: Vladimir S. Ivanov <ivvl82@gmail.com>
;; Keywords:

;;; Code:

(add-hook 'LaTeX-mode-hook
          (lambda ()
            ;; Turn on RefTeX Mode for all LaTeX files
            (turn-on-reftex)
            ;; Folding macros and environments
            (TeX-fold-mode 1)
            ;; Hide all macros and environments in the current buffer.
            ;; *NOTE* First, turn on Font-Lock mode to correct folding
            ;; if `global-font-lock-mode' enabled.
            (turn-on-font-lock-if-desired)
            (TeX-fold-buffer)
            ;; Easy typing of mathematical symbols
            (LaTeX-math-mode 1)
            ;; On-the-fly syntax checking
            (flymake-mode 1)))

;; Automatically add a quick menu of document headings
(add-hook 'reftex-mode-hook 'imenu-add-menubar-index)

(setq
 ;; Parse the buffer on load for extracting information
 TeX-parse-self t
 ;; Make RefTeX work properly with AUCTeX
 reftex-plug-into-AUCTeX t
 ;; Add \eqref to RefTeX for doing equation references
 reftex-label-alist '(AMSTeX)
 ;; Additional key bindings for RefTeX mode
 reftex-extra-bindings t
 ;; Use extra bindings with this prefix
 reftex-extra-bindings-prefix "\C-c")

(require 'tex)

;; PDF mode enable, not plain
(TeX-global-PDF-mode 1)

;; Open PDF files in Acrobat Reader
(setq TeX-view-program-list
      '(("Acroread" "acroread /a \"zoom=100 & page=%(outpage)\" %o"))
      TeX-view-program-selection '((output-pdf "Acroread")))
;; Because both X Window System and DISPLAY variable equals nil if
;; Emacs starts as a daemon, handle this case separately.
(when (daemonp)
  (delete (assoc "View" TeX-command-list) TeX-command-list)
  (add-to-list 'TeX-command-list
               '("View" "%V" TeX-run-discard-or-function
                 t t :help "Run Viewer")))

;; To use Flymake with latex or pdflatex from TeX Live distribution on Linux
(defun flymake-get-tex-args (file-name)
  (list "pdflatex" (list "-file-line-error" "-draftmode"
                         "-interaction=nonstopmode" file-name)))

;;; init-latex.el ends here
