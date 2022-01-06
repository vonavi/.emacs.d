;;; init-org-ref.el ---

;; Copyright (C) 2021  Vladimir Ivanov

;; Author: Vladimir Ivanov <ivvl82@gmail.com>
;; Keywords:

;;; Commentary:

;;

;;; Code:

;; Use Ivy completion engine for org-ref

(require 'org-ref-ivy)

(setq org-ref-insert-link-function #'org-ref-insert-link-hydra/body
      org-ref-insert-cite-function #'org-ref-cite-insert-ivy
      org-ref-insert-label-function #'org-ref-insert-label-link
      org-ref-insert-ref-function #'org-ref-insert-ref-link
      org-ref-cite-onclick-function (lambda (_) (org-ref-citation-hydra/body)))

;; Bind keyboard shortcuts
(define-key org-mode-map (kbd "C-c ]") #'org-ref-insert-link)

;;; init-org-ref.el ends here
