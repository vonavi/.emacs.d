;;; bibtex.el

(use-package bibtex
  :ensure nil
  :init
  ;; Set bibtex-completion variables
  (setq bibtex-completion-bibliography '("~/org/bibliography/references.bib")
        bibtex-completion-library-path '("~/org/bibliography/bibtex-pdfs/")))

(provide 'config/bibtex)
;;; bibtex.el ends here
