;;; org-ref.el

(use-package async)       ; required by `doi-utils-async-download-pdf'

(use-package org-ref-bibtex
  :ensure org-ref
  :init
  ;; Set bibtex-completion variables
  (setq bibtex-completion-bibliography my::bib-files
        bibtex-completion-library-path my::pdf-dirs))

(provide 'config/org-ref)
;;; org-ref.el ends here
