;;; markdown.el

(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command (executable-find "multimarkdown")))

(provide 'config/markdown)
;;; markdown.el ends here
