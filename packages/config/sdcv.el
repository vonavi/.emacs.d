;;; sdcv.el

;; Fix the absence of package versions specified by sdcv
(use-package popup)
(use-package pos-tip)
(use-package showtip)

(use-package sdcv
  :ensure-system-package sdcv
  :after (popup pos-tip showtip)
  :custom
  ;; Simple dictionary list
  (sdcv-dictionary-simple-list '("LingvoUniversal (En-Ru)"
                                 "Universal (Ru-En)"
                                 "Physics (En-Ru)"
                                 "Physics (Ru-En)"))
  ;; Complete dictionary list
  (sdcv-dictionary-complete-list '("LingvoUniversal (En-Ru)"
                                   "Universal (Ru-En)"
                                   "Physics (En-Ru)"
                                   "Physics (Ru-En)")))

(provide 'config/sdcv)
;;; sdcv.el ends here
