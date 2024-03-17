;;; org.el

;; Workaround to fetch Org-mode from a remote repository
(assq-delete-all 'org package--builtins)
(assq-delete-all 'org package--builtin-versions)

(use-package org
  :init
  (setq org-highlight-latex-and-related '(native) ; highlight inline mathematics
        org-pretty-entities t   ; display entities as UTF-8 characters
        org-src-fontify-natively t)     ; fontify code in code blocks

  ;; Set the font family for Org headings
  (set-face-attribute 'variable-pitch nil
                      :family (face-attribute 'default :family))

  :config
  ;; Default viewer for PDF files
  (add-to-list 'org-file-apps '("\\.pdf\\'" . "evince %s"))
  (add-to-list 'org-file-apps
               '("\\.pdf::\\([0-9]+\\)\\'" . "evince --page-label=%1 %s"))

  ;; Indent text according to outline structure
  :hook (org-mode . (lambda () (org-indent-mode +1))))

(use-package ol
  :ensure org
  :bind ("C-c l" . org-store-link))

(use-package org-agenda
  :ensure org
  :bind ("C-c a" . org-agenda))

(use-package ob-org
  :ensure org
  :init
  ;; Active Babel languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((C . t)
     (python . t))))

(use-package oc
  :ensure org
  :init
  (setq
   ;; Set bibliographies globally
   org-cite-global-bibliography my/bib-files
   ;; Set the directory where CSL styles are stored
   org-cite-csl-styles-dir "~/org/styles"
   ;; Automatically select the processor for exporting citations
   org-cite-export-processors '((latex biblatex)
                                (t . (csl "ieee.csl"))))

  ;; Allow to use the CSL backend
  :config (require 'oc-csl))

(use-package citar
  :init
  (setq citar-bibliography my/bib-files)
  ;; Select citation processors
  (setq org-cite-activate-processor 'citar
        org-cite-follow-processor 'citar
        org-cite-insert-processor 'citar)

  :bind
  (:map org-mode-map ("C-c b" . org-cite-insert)))

(use-package ox-latex
  :ensure org
  :init
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
     "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f")))

(provide 'config/org)
;;; org.el ends here
