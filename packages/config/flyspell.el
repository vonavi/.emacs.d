;;; flyspell.el

(use-package flyspell
  :ensure nil
  :init
  ;; This mode will use the fastest available method to come up with
  ;; decent suggestions for common misspellings.
  (setq ispell-extra-args '("--sug-mode=ultra"))
  ;; Set default dictionary
  (setq ispell-dictionary "english")

  :hook ((text-mode . turn-on-flyspell)
         (prog-mode . flyspell-prog-mode)))

(provide 'config/flyspell)
;;; flyspell.el ends here
