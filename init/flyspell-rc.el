;;; flyspell-rc.el ---

;; Copyright (C) 2013 Vladimir S. Ivanov

;; Author: Vladimir S. Ivanov <ivvl82@gmail.com>

;;; Commentary:

;;; Code:

(require 'flyspell)

;; Use spell checker Aspell as a replacement for Ispell
(setq ispell-program-name "aspell")

;; According to the aspell documentation, "ultra" is the fastest mode,
;; which is still twice as slow as ispell. If your machine is fast
;; enough, a better option might be to try "fast" mode, which is twice
;; as slow as "ultra", but more accurate. The "normal" mode, which is
;; the aspell default, is even more accurate, but is reportedly 10
;; times slower than "fast" mode, and on my machine it makes editing
;; and motion around the buffer noticeably sluggish when in
;; flyspell-mode. YMMV, depending on how fast your machine is, and how
;; big your word-lists are.
(setq ispell-extra-args '("--sug-mode=ultra"))

;; Some extra flyspell delayed command
(mapcar 'flyspell-delay-command '(scroll-up1 scroll-down1))

;; Set default dictionary
(setq ispell-dictionary "english")

;; Auto-start of Flyspell
(add-hook 'text-mode-hook 'turn-on-flyspell)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(add-hook 'text-mode-hook 'my-change-dictionary)

(defun my-change-dictionary ()
  "Set Russian language for \"ru\" directories."
  (when (and buffer-file-name
             (string-match-p "/ru/" buffer-file-name))
    (set-input-method "russian-computer")
    (ispell-change-dictionary "ru")))

;;; flyspell-rc.el ends here
