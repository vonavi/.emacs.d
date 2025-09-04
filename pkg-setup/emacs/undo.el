;;; emacs/undo.el

;;
;;; Persistent undo, available between sessions

(use-package undo-fu-session
  :config (global-undo-fu-session-mode +1))

;;
;;; Visual undo tree

(use-package vundo
  :custom
  ;; Take less on-screen space
  (vundo-compact-display t)
  ;; Use unicode instead of ascii characters to display the tree
  (vundo-glyph-alist vundo-unicode-symbols)
  :bind
  ([remap undo] . vundo))

(provide 'emacs/undo)
;;; emacs/undo.el ends here
