;;; proof-general.el

(use-package proof-general
  :custom
  (proof-script-fly-past-comments t) ; ignore comments when processing a proof
  (proof-splash-enable nil)      ; disable Proof General splash screen
  (proof-three-window-mode-policy 'hybrid)) ; enable three windows mode

(use-package coq-mode
  :ensure proof-general
  :after undo-tree
  ;; Do not indicate the proof position by arrow
  :init (setq overlay-arrow-string "")
  :hook (coq-mode . (lambda () (undo-tree-mode +1)))
  :mode "\\.v\\'")

(provide 'config/proof-general)
;;; proof-general.el ends here
