;;; org-roam.el

(use-package org-roam
  :after org
  :init
  (setq org-roam-directory     ; directory to store the Org-roam files
        (concat (file-name-as-directory org-directory) "roam"))
  ;; Show the tags associated with the note
  (setq org-roam-node-display-template
        (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))

  :config
  ;; Update SQLite database of Org files as they are modified
  (org-roam-db-autosync-mode +1))

(use-package org-roam-ui
  :delight)

(provide 'config/org-roam)
;;; org-roam.el ends here
