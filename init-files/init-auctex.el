;;; init-auctex.el ---

;; Copyright (C) 2015  Vladimir Ivanov

;; Author: Vladimir Ivanov <ivvl82@gmail.com>
;; Keywords:

;;; Commentary:

;;

;;; Code:

(require 'latex)

(add-hook 'LaTeX-mode-hook 'my:LaTeX-init)
(defun my:LaTeX-init ()
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
  (LaTeX-math-mode 1)
  ;; Forward and inverse search
  (TeX-source-correlate-mode 1)
  ;; Set the favorite environment
  (setq LaTeX-default-environment "align"))

(setq
 ;; Parse the buffer on load for extracting information
 TeX-parse-self t
 ;; Auto save before compiling
 TeX-save-query nil
 ;; Start a correlation server without asking
 TeX-source-correlate-start-server t)

;; PDF mode enable, not plain
(TeX-global-PDF-mode 1)

;; Open PDF files in Evince
(setq TeX-view-program-list
      '(("Evince" "evince --page-index=%(outpage) %o"))
      TeX-view-program-selection '((output-pdf "Evince")))

;; Fold a math macro automatically after it's inserted
(setq LaTeX-math-insert-function
      (lambda (string)
        (TeX-insert-macro string)
        (save-excursion (backward-char) (TeX-fold-item 'math))))

;;-------
;; RefTeX
;;-------

(setq
 ;; Make RefTeX work properly with AUCTeX
 reftex-plug-into-AUCTeX t
 ;; Add \eqref to RefTeX for doing equation references
 reftex-label-alist '(AMSTeX)
 ;; Additional key bindings for RefTeX mode
 reftex-extra-bindings t)

;; Automatically add a quick menu of document headings
(add-hook 'reftex-mode-hook 'imenu-add-menubar-index)

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
(defadvice reftex-label (after my:TeX-fold-label activate)
  (save-excursion (backward-char) (TeX-fold-item 'macro)))
(defadvice reftex-reference (after my:TeX-fold-reference activate)
  (save-excursion (backward-char) (TeX-fold-item 'macro)))
(defadvice reftex-citation (after my:TeX-fold-citation activate)
  (save-excursion (backward-char) (TeX-fold-item 'macro)))

;;; init-auctex.el ends here
