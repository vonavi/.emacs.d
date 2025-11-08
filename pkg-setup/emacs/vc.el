;;; emacs/vc.el

;;
;;; Get the GitHub/Bitbucket/GitLab URL for a buffer location

(use-package git-link
  :bind (:map vc-prefix-map
         ("c" . git-link-commit)
         ("o" . git-link)))

(provide 'emacs/vc)
;;; emacs/vc.el ends here
