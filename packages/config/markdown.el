;;; markdown.el

(use-package markdown-mode
  :ensure-system-package (multimarkdown . libtext-multimarkdown-perl)
  :custom (markdown-command (executable-find "multimarkdown"))
  :mode ("README\\.md\\'" . gfm-mode))

(provide 'config/markdown)
;;; markdown.el ends here
