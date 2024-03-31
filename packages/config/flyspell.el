;;; flyspell.el

(use-package flyspell
  :ensure nil
  :ensure-system-package hunspell
  :init
  (setq ispell-program-name (executable-find "hunspell")
        ispell-local-dictionary "en_US") ; the default dictionary

  :hook ((text-mode . turn-on-flyspell)
         (prog-mode . flyspell-prog-mode)))

(provide 'config/flyspell)
;;; flyspell.el ends here
