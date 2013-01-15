;;; init.el ---

;; Copyright (C) Vladimir S. Ivanov
;;
;; Author: Vladimir S. Ivanov <ivvl82@gmail.com>
;; Keywords:
;; Requirements:
;; Status: not intended to be distributed yet


;;-----------------------
;; Emacs appearance setup
;;-----------------------

(add-to-list 'default-frame-alist '(font . "Consolas 14")) ; set default font
(blink-cursor-mode 0) ; no blinking cursor
(setq inhibit-splash-screen t) ; prevent silly initial splash screen
(tool-bar-mode 0) ; no tool bar
(set-scroll-bar-mode 'right) ; place scroll bar
(global-font-lock-mode 1) ; use colors to highlight commands, etc.

;; Install a chosen color theme
(require 'color-theme)
(color-theme-initialize)
(color-theme-standard)

;; Create a reasonable title bar
(add-hook 'window-configuration-change-hook
          (lambda ()
            (setq frame-title-format
                  (concat
                   invocation-name "@" system-name ": "
                   (replace-regexp-in-string
                    (concat "/home/" user-login-name) "~"
                    (or buffer-file-name "%b"))))))

;; Mode bar preferences
(column-number-mode 1) ; show column number in mode-line
(setq display-time-day-and-date t ; display the day and date in the mode line
      display-time-24hr-format t ; use 24hr format
      display-time-interval 10 ; redisplay every ten seconds
      display-time-default-load-average nil) ; don't display the system load average
(display-time)

;; If everything is OK, then a server starts
(server-start)

;;; init.el ends here
