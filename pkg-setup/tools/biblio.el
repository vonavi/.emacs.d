;;; tools/biblio.el

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

;;
;;; Citation-related commands for org, latex, markdown

(use-package citar
  :after (bibtex-completion oc)
  :custom
  (citar-bibliography bibtex-completion-bibliography)
  (citar-library-paths bibtex-completion-library-path)
  ;; Select citation processors
  (org-cite-activate-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-insert-processor 'citar)
  ;; How to open files with given extensions
  (citar-file-open-functions '(("html" . citar-file-open-external)
                               ("pdf" . citar-file-open-external)
                               (t . find-file))))

;;
;;; Citar/org-roam integration

(use-package citar-org-roam
  :after org-roam
  :delight
  :custom
  ;; Default note title output
  (citar-org-roam-note-title-template
   "${author editor:%etal} (${year issued date}) ${title}")
  ;; Directory of literature notes
  (citar-org-roam-subdir "references/")
  :config
  (citar-org-roam-mode +1))

;;
;;; A BibTeX backend for completion frameworks

(use-package bibtex-completion
  :after org
  :custom
  ;; Set BibTeX-completion variables
  (bibtex-completion-bibliography
   (list (expand-file-name "bibliography/references.bib" org-directory)))
  (bibtex-completion-library-path
   (list (expand-file-name "bibliography/bibtex-pdfs/" org-directory)))
  ;; Look at this field of BibTeX entry to figure out which PDF to open
  (bibtex-completion-pdf-field "file"))

(provide 'tools/biblio)
;;; tools/biblio.el ends here
