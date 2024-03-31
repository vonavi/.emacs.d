;;; markdown.el

(use-package markdown-mode
  :ensure-system-package (multimarkdown . libtext-multimarkdown-perl)
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command (executable-find "multimarkdown")))

(provide 'config/markdown)
;;; markdown.el ends here
