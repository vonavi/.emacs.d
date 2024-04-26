;;; mouse.el

(use-package mouse
  :ensure nil
  :init (setq mouse-yank-at-point t))   ; paste at point NOT at cursor

(use-package mwheel
  :ensure nil
  :init
  (setq mouse-wheel-scroll-amount '(1) ; mouse scroll one line at a time
        mouse-wheel-progressive-speed t ; don't accelerate scrolling
        mouse-wheel-follow-mouse t)     ; scroll window under mouse
  :config
  (mouse-wheel-mode +1))

(provide 'config/mouse)
;;; mouse.el ends here
