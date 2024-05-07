;;; display-line-numbers.el

(use-package display-line-numbers
  :init
  (setq-default
   display-line-numbers-widen t  ; use absolute numbers in narrowed buffers
   display-line-numbers-width 3) ; less jitter when scrolling
  :hook (prog-mode . (lambda () (display-line-numbers-mode +1))))

(provide 'config/display-line-numbers)
;;; display-line-numbers.el ends here
