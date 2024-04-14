;;; citar.el

(use-package citar
  :after (bibtex oc)
  :init
  (setq citar-bibliography bibtex-completion-bibliography
        citar-library-paths bibtex-completion-library-path)
  ;; Select citation processors
  (setq org-cite-activate-processor 'citar
        org-cite-follow-processor 'citar
        org-cite-insert-processor 'citar)
  ;; How to open files with given extensions
  (setq citar-file-open-functions '(("html" . citar-file-open-external)
                                    ("pdf" . citar-file-open-external)
                                    (t . find-file))))

(use-package citar-org-roam
  :after org-roam
  :delight
  :init
  (setq citar-org-roam-note-title-template ; the default note title output
        "${author editor:%etal} (${year issued date}) ${title}"
        citar-org-roam-subdir      ; the directory of literature notes
        "references/")
  :config (citar-org-roam-mode +1))

(provide 'config/citar)
;;; citar.el ends here
