;;; org-ref.el

(use-package async)       ; required by `doi-utils-async-download-pdf'

(use-package org-ref-bibtex
  :ensure org-ref
  :after (oc org)
  :init
  ;; Set bibtex-completion variables
  (setq bibtex-completion-bibliography org-cite-global-bibliography
        bibtex-completion-library-path
        `(,(concat (file-name-as-directory org-directory)
                   "bibliography/bibtex-pdfs/"))))

(provide 'config/org-ref)
;;; org-ref.el ends here
