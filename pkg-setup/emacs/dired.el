;;; emacs/dired.el

(use-package dired
  :ensure nil
  ;; Kill the current buffer when selecting a new directory
  :custom (dired-kill-when-opening-new-dired-buffer t))

(use-package dired-x
  :ensure nil
  :custom
  ;; Do not show messages when omitting files
  (dired-omit-verbose nil)
  ;; Hide unnecessary files in Dired
  (dired-omit-files
   (concat "\\`[.]?#\\|\\`[.][.]?\\'"
           "\\|^\\.git\\'"
           "\\|^\\.\\(?:mypy_cache\\|ruff_cache\\|venv\\)\\'"))
  :hook
  (dired-mode . dired-omit-mode))

(provide 'emacs/dired)
;;; emacs/dired.el ends here
