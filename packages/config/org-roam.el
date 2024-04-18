;;; org-roam.el

(use-package org-roam
  :ensure-system-package (dot . graphviz)
  :after org
  :init
  (setq org-roam-directory     ; directory to store the Org-roam files
        (concat (file-name-as-directory org-directory) "roam"))
  ;; Show the tags associated with the note
  (setq org-roam-node-display-template
        (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  ;; Update SQLite database of Org files as they are modified
  (org-roam-db-autosync-mode +1))

(use-package org-roam-ui
  :delight)

(provide 'config/org-roam)
;;; org-roam.el ends here
