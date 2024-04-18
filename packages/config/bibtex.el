;;; bibtex.el

(use-package bibtex
  :ensure nil
  :after org
  :init
  ;; Set bibtex-completion variables
  (setq bibtex-completion-bibliography
        `(,(expand-file-name "bibliography/references.bib" org-directory))
        bibtex-completion-library-path
        `(,(expand-file-name "bibliography/bibtex-pdfs/" org-directory))))

(provide 'config/bibtex)
;;; bibtex.el ends here
