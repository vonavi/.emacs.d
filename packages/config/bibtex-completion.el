;;; bibtex-completion.el

(use-package bibtex-completion
  :after org
  :custom
  ;; Set BibTeX-completion variables
  (bibtex-completion-bibliography
   (list (expand-file-name "bibliography/references.bib" org-directory)))
  (bibtex-completion-library-path
   (list (expand-file-name "bibliography/bibtex-pdfs/" org-directory))))

(provide 'config/bibtex-completion)
;;; bibtex-completion.el ends here
