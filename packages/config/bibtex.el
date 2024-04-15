;;; bibtex.el

(use-package bibtex
  :ensure nil
  :after org
  :init
  ;; Set bibtex-completion variables
  (setq bibtex-completion-bibliography
        `(,(concat (file-name-as-directory org-directory)
                   "bibliography/references.bib"))
        bibtex-completion-library-path
        `(,(concat (file-name-as-directory org-directory)
                   "bibliography/bibtex-pdfs/"))))

(provide 'config/bibtex)
;;; bibtex.el ends here
