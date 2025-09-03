;;; citar.el

(use-package citar
  :after (bibtex oc)
  :custom
  (citar-bibliography bibtex-completion-bibliography)
  (citar-library-paths bibtex-completion-library-path)
  ;; Select citation processors
  (org-cite-activate-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-insert-processor 'citar)
  ;; How to open files with given extensions
  (citar-file-open-functions '(("html" . citar-file-open-external)
                               ("pdf" . citar-file-open-external)
                               (t . find-file))))

(use-package citar-org-roam
  :after org-roam
  :delight
  :custom
  ;; Default note title output
  (citar-org-roam-note-title-template
   "${author editor:%etal} (${year issued date}) ${title}")
  ;; Directory of literature notes
  (citar-org-roam-subdir "references/")
  :config
  (citar-org-roam-mode +1))

(provide 'config/citar)
;;; citar.el ends here
