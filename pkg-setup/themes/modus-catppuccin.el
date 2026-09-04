;;; themes/modus-catppuccin.el

;;
;;; Collection of highly accessible, elegantly designed, extensive, and fully
;;; customisable themes

(use-package modus-themes
  :custom
  ;; Use italics for comments and doc strings.
  (modus-themes-italic-constructs t)
  ;; Better heading structure
  (modus-themes-headings
   '((0 . (1.5))
     (1 . (1.3))
     (2 . (1.18))
     (3 . (1.08))
     (4 . (1.0)))))

;;
;;; Themes for Emacs based on the Catppuccin palette, built on modus-themes

(use-package modus-catppuccin
  :straight (:type git
             :host gitlab
             :repo "magus/modus-catppuccin"
             :branch "main")
  :after modus-themes
  :preface
  (defun my/load-theme (&optional frame)
    "Load the `catppuccin-frappe' theme in FRAME (defaults to selected)."
    (with-selected-frame (or frame (selected-frame))
      (modus-themes-load-theme 'catppuccin-frappe)))
  :config
  (if (daemonp)
      (add-hook 'after-make-frame-functions #'my/load-theme)
    (my/load-theme)))

(provide 'themes/modus-catppuccin)
;;; themes/modus-catppuccin.el ends here
