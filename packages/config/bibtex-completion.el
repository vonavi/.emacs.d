;;; bibtex-completion.el

(use-package bibtex-completion
  :after org
  :init
  ;; Set bibtex-completion variables
  (setq bibtex-completion-bibliography
        `(,(expand-file-name "bibliography/references.bib" org-directory))
        bibtex-completion-library-path
        `(,(expand-file-name "bibliography/bibtex-pdfs/" org-directory))))

(provide 'config/bibtex-completion)
;;; bibtex-completion.el ends here
