;;; auctex.el ---

;; Copyright (C) 2024  Vladimir Ivanov

;; Author: Vladimir Ivanov <ivvl82@gmail.com>
;; Keywords:

;;; Commentary:

;;

;;; Code:

(defun my:LaTeX-init ()
  ;; Folding macros and environments
  (TeX-fold-mode +1)
  ;; Hide all macros and environments in the current buffer.
  ;; *NOTE* Turn on Font-Lock mode first to correct folding if
  ;; `global-font-lock-mode' enabled.
  (turn-on-font-lock-if-desired)
  (TeX-fold-buffer)
  ;; Easy typing of mathematical symbols
  (LaTeX-math-mode +1)
  ;; Forward and inverse search
  (TeX-source-correlate-mode +1))

(use-package auctex
  :init
  (setq
   ;; Parse the buffer on load for extracting information
   TeX-parse-self t
   ;; Auto save before compiling
   TeX-save-query nil
   ;; Start a correlation server without asking
   TeX-source-correlate-start-server t
   ;; Function for reading \includegraphics files
   LaTeX-includegraphics-read-file 'LaTeX-includegraphics-read-file-relative
   ;; Strip known extensions from image file name
   LaTeX-includegraphics-strip-extension-flag nil
   ;; Insert paired symbols for opening and closing inline equation
   TeX-electric-math '("$" . "$"))

  ;; Fold a math macro automatically after it's inserted
  (setq LaTeX-math-insert-function
        (lambda (string)
          (TeX-insert-macro string)
          (save-excursion (backward-char) (TeX-fold-item 'math))))

  ;; Open PDF files in Evince
  (setq TeX-view-program-list
        '(("Evince" "evince --page-index=%(outpage) %o"))
        TeX-view-program-selection '((output-pdf "Evince")))

  ;; RefTeX options
  (setq
   ;; Make RefTeX work properly with AUCTeX
   reftex-plug-into-AUCTeX t
   ;; Add \eqref to RefTeX for doing equation references
   reftex-label-alist '(AMSTeX)
   ;; Additional key bindings for RefTeX mode
   reftex-extra-bindings t)

  ;; Fold a RefTeX macro automatically after it's inserted
  (defun my:RefTeX-fold ()
    (save-excursion (backward-char) (TeX-fold-item 'macro)))
  (advice-add 'reftex-label :after #'my:RefTeX-fold)
  (advice-add 'reftex-reference :after #'my:RefTeX-fold)
  (advice-add 'reftex-citation :after #'my:RefTeX-fold)

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

  :config
  ;; Enable PDF mode, not plain
  (TeX-global-PDF-mode +1)

  :hook
  ((LaTeX-mode . my:LaTeX-init)
   ;; Support for LaTeX labels, references, citations and index entries
   (LaTeX-mode . turn-on-reftex)
   ;; Automatically add a quick menu of document headings
   (reftex-mode . imenu-add-menubar-index)))

(provide 'config/auctex)
;;; auctex.el ends here
