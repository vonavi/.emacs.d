;;; org.el ---

;; Copyright (C) 2024  Vladimir Ivanov

;; Author: Vladimir Ivanov <ivvl82@gmail.com>
;; Keywords:

;;; Commentary:

;;

;;; Code:

(use-package org
  :init
  (setq org-highlight-latex-and-related '(native) ; highlight inline mathematics
        org-pretty-entities t   ; display entities as UTF-8 characters
        org-src-fontify-natively t)     ; fontify code in code blocks

  ;; Set the font family for Org headings
  (set-face-attribute 'variable-pitch nil
                      :family (face-attribute 'default :family))

  :config
  ;; Keyboard shortcuts
  (require 'org-agenda)
  (bind-keys ("C-c a" . org-agenda)
             ("C-c l" . org-store-link))

  ;; Default viewer for HTML files
  (add-to-list 'org-file-apps
               '("\\.x?html?\\'" . (lambda (file path) (browse-url file))))
  ;; Default viewer for PDF files
  (add-to-list 'org-file-apps '("\\.pdf\\'" . "evince %s"))
  (add-to-list 'org-file-apps
               '("\\.pdf::\\([0-9]+\\)\\'" . "evince --page-label=%1 %s"))

  ;; Active Babel languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((C . t)
     (python . t)))

  ;; To use the listings package automatically for LaTeX documents
  (setq org-latex-listings 'minted)
  ;; List of additional LaTeX packages
  (add-to-list 'org-latex-packages-alist '("" "listings"))
  (add-to-list 'org-latex-packages-alist '("" "color"))
  (add-to-list 'org-latex-packages-alist '("" "minted"))
  (add-to-list 'org-latex-packages-alist '("" "cmap"))
  (add-to-list 'org-latex-packages-alist '("english, russian" "babel"))
  ;; Listings package options
  (setq org-latex-listings-options
        '(("inputencoding" "utf8")
          ("extendedchars" "\\true")
          ("keepspaces" "true")
          ("basicstyle" "\\ttfamily")
          ("columns" "flexible")
          ("showstringspaces" "false")))
  ;; Minted package options
  (setq org-latex-minted-options '(("encoding" "utf8")))

  ;; Customize how Org-mode produces a PDF file
  (setq
   org-latex-pdf-process
   '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
     "bibtex %b"
     "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
     "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

  ;; Indent text according to outline structure
  :hook (org-mode . (lambda () (org-indent-mode +1))))

(provide 'config/org)
;;; org.el ends here
