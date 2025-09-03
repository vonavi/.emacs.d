;;; org.el

(use-package org
  :ensure-system-package (xdg-open . xdg-utils)
  :custom
  (org-highlight-latex-and-related '(native)) ; highlight inline mathematics
  (org-pretty-entities t)       ; display entities as UTF-8 characters
  (org-src-fontify-natively t) ; use the language major mode to fontify code
  (org-src-tab-acts-natively t) ; use the language major mode to indent code
  :custom-face
  ;; Set the font family for Org headings
  (variable-pitch ((t (:family ,(face-attribute 'default :family)))))
  :config
  ;; Use "xdg-open" to open files by default
  (setcdr (assq t org-file-apps-gnu) "xdg-open %s"))

(use-package org-indent
  :ensure org
  :delight
  ;; Indent text according to outline structure
  :hook (org-mode . (lambda () (org-indent-mode +1))))

(use-package org-id
  :ensure org
  ;; Do not create ID if a CUSTOM_ID exists
  :custom (org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id))

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
  :after bibtex-completion
  :custom
  ;; Set bibliographies globally
  (org-cite-global-bibliography bibtex-completion-bibliography)
  ;; Set the directory where CSL styles are stored
  (org-cite-csl-styles-dir (expand-file-name "styles/" org-directory))
  ;; Automatically select the processor for exporting citations
  (org-cite-export-processors '((latex . (bibtex "ieeetr"))
                                (t . (csl "ieee.csl"))))
  :bind
  (:map org-mode-map ("C-c b" . org-cite-insert))
  ;; Allow to use the CSL backend
  :config (require 'oc-csl))

(use-package ox-latex
  :ensure org
  :ensure-system-package
  (latexmk
   (pdflatex . texlive-latex-base)
   (pygmentize . python3-pygments))
  :custom
  ;; Choose the Minted package for LaTeX documents
  (org-latex-src-block-backend 'minted)
  ;; Minted package options
  (org-latex-minted-options '(("encoding" "utf8")))
  ;; Customize how Org-mode produces a PDF file
  (org-latex-pdf-process
   '("latexmk -shell-escape -f -pdf -%latex -interaction=nonstopmode -output-directory=%o %f"))
  :init
  ;; List of additional LaTeX packages
  (add-to-list 'org-latex-packages-alist '("" "minted"))
  (add-to-list 'org-latex-packages-alist '("" "cmap"))
  (add-to-list 'org-latex-packages-alist '("english,russian" "babel"))
  (add-to-list 'org-latex-packages-alist '("T2A" "fontenc"))
  (add-to-list 'org-latex-packages-alist '("parfill" "parskip")))

(use-package org-protocol
  :ensure org
  :requires server)

(provide 'config/org)
;;; org.el ends here
