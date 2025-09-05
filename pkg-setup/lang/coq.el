;;; lang/coq.el

;;
;;; A generic Emacs interface for proof assistants

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

(provide 'lang/coq)
;;; lang/coq.el ends here
