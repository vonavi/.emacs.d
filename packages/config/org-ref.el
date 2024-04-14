;;; org-ref.el

(use-package async)       ; required by `doi-utils-async-download-pdf'

(use-package org-ref-bibtex
  :ensure org-ref
  :after bibtex)

(provide 'config/org-ref)
;;; org-ref.el ends here
