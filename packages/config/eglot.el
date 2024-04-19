;;; eglot.el

(use-package eglot
  :ensure nil
  :ensure-system-package (pylsp . python3-pylsp)
  :hook (python-mode . eglot-ensure))

(use-package corfu
  :init (setq corfu-auto t)             ; enable auto completion
  :config (global-corfu-mode +1))

(use-package cape
  :config
  ;; Enable cache busting, depending on if your server returns
  ;; sufficiently many candidates in the first place.
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster))

(provide 'config/eglot)
;;; eglot.el ends here
