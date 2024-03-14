;;; pomm.el

(use-package pomm
  :init (setq pomm-audio-enabled t)
  ;; Display the timer in the modeline
  :config (pomm-mode-line-mode +1))

(provide 'config/pomm)
;;; pomm.el ends here
