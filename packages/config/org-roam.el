;;; org-roam.el

(use-package org-roam
  :ensure-system-package (dot . graphviz)
  :after org
  :custom
  ;; Set the directory to store Org-roam files
  (org-roam-directory (expand-file-name "roam/" org-directory))
  ;; Show the tags associated with the note
  (org-roam-node-display-template
   (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  :bind
  (("C-c n f" . org-roam-node-find)
   ("C-c n g" . org-roam-graph)
   ("C-c n c" . org-roam-capture)
   ("C-c n j" . org-roam-dailies-capture-today)
   :map org-mode-map
   ("C-c n t" . org-roam-tag-add)
   ("C-c n T" . org-roam-tag-remove)
   ("C-c n l" . org-roam-buffer-toggle)
   ("C-c n i" . org-roam-node-insert)
   ("C-c n r" . org-roam-refile)
   ("C-c n x" . org-roam-extract-subtree))
  :config
  ;; Update SQLite database of Org files as they are modified
  (org-roam-db-autosync-mode +1))

(use-package org-roam-ui
  :delight)

(provide 'config/org-roam)
;;; org-roam.el ends here
