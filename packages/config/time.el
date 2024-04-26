;;; time.el

(use-package time
  :ensure nil
  :init
  (setq display-time-day-and-date t ; display the day and date in the mode line
        display-time-24hr-format t  ; use 24hr format
        display-time-interval 10    ; redisplay every ten seconds
        display-time-default-load-average nil) ; don't display load average
  :config
  (display-time-mode +1))

(provide 'config/time)
;;; time.el ends here
