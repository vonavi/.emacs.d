;;; init-flyspell.el ---

;; Copyright (C) 2013 Vladimir S. Ivanov

;; Author: Vladimir S. Ivanov <ivvl82@gmail.com>
;; Keywords:

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
;; Make the Russian dictionary available to 'ispell-change-dictionary'
(add-to-list 'ispell-dictionary-alist
             '("russian"
               "[АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЬЫЪЭЮЯабвгдеёжзийклмнопрстуфхцчшщьыъэюя]"
               "[^АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЬЫЪЭЮЯабвгдеёжзийклмнопрстуфхцчшщьыъэюя]"
               "" nil ("-d" "ru") nil utf-8))

;; Auto-start of Flyspell
(add-hook 'text-mode-hook 'turn-on-flyspell)
(mapcar (lambda (mode-hook) (add-hook mode-hook 'flyspell-prog-mode))
        '(c-mode-common-hook css-mode-hook emacs-lisp-mode-hook
                             js-mode-hook ruby-mode-hook sh-mode-hook))

;;; init-flyspell.el ends here
