;;; display-line-numbers.el

(use-package display-line-numbers
  :custom
  (display-line-numbers-type 'relative) ; relative line numbering
  (display-line-numbers-widen t) ; absolute numbers in narrowed buffers
  (display-line-numbers-width 3) ; less jitter when scrolling
  :hook
  (prog-mode . (lambda () (display-line-numbers-mode +1))))

(provide 'config/display-line-numbers)
;;; display-line-numbers.el ends here
