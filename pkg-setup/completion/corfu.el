;;; completion/corfu.el

;;
;;; COmpletion in Region FUnction

(use-package corfu
  :custom (corfu-auto t)                ; enable auto completion
  :config (global-corfu-mode +1))

;;
;;; Completion At Point Extensions

(use-package cape
  :config
  ;; Enable cache busting, depending on if your server returns
  ;; sufficiently many candidates in the first place.
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster))

(provide 'completion/corfu)
;;; completion/corfu.el ends here
