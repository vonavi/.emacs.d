;;; sdcv.el

;; Fix the absence of package versions specified by sdcv
(use-package popup)
(use-package pos-tip)
(use-package showtip)

(use-package sdcv
  :after (popup pos-tip showtip)
  :init
  ;; Simple dictionary list
  (setq sdcv-dictionary-simple-list
        '("LingvoUniversal (En-Ru)"
          "Universal (Ru-En)"
          "Physics (En-Ru)"
          "Physics (Ru-En)"))
  ;; Complete dictionary list
  (setq sdcv-dictionary-complete-list
        '("LingvoUniversal (En-Ru)"
          "Universal (Ru-En)"
          "Physics (En-Ru)"
          "Physics (Ru-En)")))

(provide 'config/sdcv)
;;; sdcv.el ends here
