;;; init-latex.el ---

;; Copyright (C) 2013 Vladimir S. Ivanov

;; Author: Vladimir S. Ivanov <ivvl82@gmail.com>
;; Keywords:

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
  (LaTeX-math-mode 1)
  ;; On-the-fly syntax checking
  (flymake-mode 1))

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

;; Automatically add a quick menu of document headings
(add-hook 'reftex-mode-hook 'imenu-add-menubar-index)

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

;; Use Flymake with pdflatex from TeX Live distribution on Linux
(require 'flymake)
(defun flymake-get-tex-args (file-name)
  `("pdflatex" ("-file-line-error" "-draftmode"
                "-interaction=nonstopmode" ,file-name)))

;; Auto insert a tilde before the \cite macro if the preceding
;; character isn't whitespace or a tilde
(setq reftex-format-cite-function
      (lambda (key fmt)
        (let ((cite (replace-regexp-in-string "%l" key fmt)))
          (if (or
               ;; Check if there is a tilde or already a cite command
               (member (string-to-char fmt) '(?~ ?,))
               ;; Check if the preceding character is a whitespace or tilde
               (member (preceding-char) '(?\ ?\t ?\n ?~)))
              cite
            (concat "~" cite)))))

;; Fold a RefTeX macro automatically after it's inserted
(defadvice reftex-label (after TeX-fold-label activate)
  (save-excursion (backward-char) (TeX-fold-item 'macro)))
(defadvice reftex-reference (after TeX-fold-reference activate)
  (save-excursion (backward-char) (TeX-fold-item 'macro)))
(defadvice reftex-citation (after TeX-fold-citation activate)
  (save-excursion (backward-char) (TeX-fold-item 'macro)))

;; Fold a math macro automatically after it's inserted
(require 'latex)
(defun LaTeX-math-insert (string dollar)
  "Insert \\STRING{}.  If DOLLAR is non-nil, put $'s around it."
  (if dollar (insert "$"))
  (funcall LaTeX-math-insert-function string)
  (save-excursion (backward-char) (TeX-fold-item 'math))
  (if dollar (insert "$")))

;;; init-latex.el ends here
