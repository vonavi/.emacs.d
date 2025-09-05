;;; lang/markdown.el

;;
;;; Major mode for Markdown-formatted text

(use-package markdown-mode
  :ensure-system-package (multimarkdown . libtext-multimarkdown-perl)
  :mode ("README\\.md\\'" . gfm-mode)
  :custom (markdown-command (executable-find "multimarkdown")))

(provide 'lang/markdown)
;;; lang/markdown.el ends here
