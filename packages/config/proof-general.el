;;; proof-general.el ---

;; Copyright (C) 2023  Vladimir Ivanov

;; Author: Vladimir Ivanov <ivvl82@gmail.com>
;; Keywords:

;;; Commentary:

;;

;;; Code:

(use-package proof-general
  :init
  (setq proof-script-fly-past-comments t ; ignore comments when processing a proof
        proof-splash-enable nil          ; disable Proof General splash screen
        proof-three-window-mode-policy 'hybrid)) ; enable three windows mode

(use-package coq-mode
  :ensure nil
  :mode "\\.v\\'")

(provide 'config/proof-general)

;;; proof-general.el ends here
