;;; latex-rc.el ---

;; Copyright (C) 2013 Vladimir Ivanov

;; Author: Vladimir Ivanov <ivvl82@gmail.com>

;;; Commentary:

;;; Code:

(add-hook 'LaTeX-mode-hook 'my-LaTeX-init)
(defun my-LaTeX-init ()
  ;; Turn on RefTeX Mode for all LaTeX files
  (turn-on-reftex)
  ;; Folding macros and environments
  (TeX-fold-mode 1)
  ;; Hide all macros and environments in the current buffer.
  ;; *NOTE* Turn on Font-Lock mode first to correct folding if
  ;; `global-font-lock-mode' enabled.
  (turn-on-font-lock-if-desired)
  (TeX-fold-buffer)
  ;; Easy typing of mathematical symbols
  (LaTeX-math-mode 1))

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
 reftex-extra-bindings-prefix "\C-c"
 ;; Auto save before compiling
 TeX-save-query nil)

;; Automatically add a quick menu of document headings
(add-hook 'reftex-mode-hook 'imenu-add-menubar-index)

(require 'tex)

;; PDF mode enable, not plain
(TeX-global-PDF-mode 1)

;; Open PDF files in Acrobat Reader
(setq TeX-view-program-list
      '(("Acroread" "acroread /a \"zoom=100 & page=%(outpage)\" %o"))
      TeX-view-program-selection '((output-pdf "Acroread")))

;; Auto insert a tilde before the \cite macro if the preceding
;; character isn't whitespace or a tilde
(setq reftex-format-cite-function
      (lambda (key fmt)
        (concat
         (unless (or
                  ;; We are inside a cite command
                  (some (apply-partially 'string= fmt)
                        '("%l" ",%l" "%l," ",%l,"))
                  ;; There is already a tilde
                  (= ?~ (string-to-char fmt))
                  ;; The preceding character is a whitespace or tilde
                  (member (preceding-char) '(?\ ?\t ?\n ?~)))
           "~")
         (replace-regexp-in-string "%l" key fmt))))

;; Fold a RefTeX macro automatically after it's inserted
(defadvice reftex-label (after my-TeX-fold-label activate)
  (save-excursion (backward-char) (TeX-fold-item 'macro)))
(defadvice reftex-reference (after my-TeX-fold-reference activate)
  (save-excursion (backward-char) (TeX-fold-item 'macro)))
(defadvice reftex-citation (after my-TeX-fold-citation activate)
  (save-excursion (backward-char) (TeX-fold-item 'macro)))

;; Fold a math macro automatically after it's inserted
(require 'latex)
(defun LaTeX-math-insert (string dollar)
  "Insert \\STRING{}.  If DOLLAR is non-nil, put $'s around it."
  (if dollar (insert "$"))
  (funcall LaTeX-math-insert-function string)
  (save-excursion (backward-char) (TeX-fold-item 'math))
  (if dollar (insert "$")))

;;; latex-rc.el ends here
