;;; org-cite.el ---

;; Copyright (C) 2024  Vladimir Ivanov

;; Author: Vladimir Ivanov <ivvl82@gmail.com>
;; Keywords:

;;; Commentary:

;;

;;; Code:

(use-package oc
  :ensure org
  :init
  (setq
   ;; Set bibliographies globally
   org-cite-global-bibliography '("~/org/bibliography/references.bib")
   ;; Set the directory where CSL styles are stored
   org-cite-csl-styles-dir "~/org/styles"
   ;; Automatically select the processor for exporting citations
   org-cite-export-processors '((latex biblatex)
                                (t . (csl "ieee.csl"))))

  ;; Allow to use the CSL backend
  :config (require 'oc-csl))

(use-package citar
  :init
  (setq citar-bibliography org-cite-global-bibliography)
  ;; Select citation processors
  (setq org-cite-activate-processor 'citar
        org-cite-follow-processor 'citar
        org-cite-insert-processor 'citar)

  :bind
  (:map org-mode-map ("C-c b" . #'org-cite-insert)))

(provide 'config/org-cite)
;;; org-cite.el ends here
