;;; proof-general.el

(use-package proof-general
  :custom
  (proof-script-fly-past-comments t) ; ignore comments when processing a proof
  (proof-splash-enable nil)      ; disable Proof General splash screen
  (proof-three-window-mode-policy 'hybrid)) ; enable three windows mode

(use-package coq-mode
  :ensure proof-general
  :after vundo
  :mode "\\.v\\'"
  ;; Do not indicate the proof position by arrow
  :init (setq overlay-arrow-string "")
  :bind (:map coq-mode-map ("C-x u" . vundo)))

(provide 'config/proof-general)
;;; proof-general.el ends here
