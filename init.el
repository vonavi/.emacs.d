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

;;-----------------
;; Emacs feel setup
;;-----------------

;; Enable mouse wheel
(mouse-wheel-mode 1)
(setq mouse-wheel-scroll-amount '(1)    ; mouse scroll one line at a time
      mouse-wheel-progressive-speed nil ; don't accelerate scrolling
      mouse-wheel-follow-mouse t        ; scroll window under mouse
      setscroll-step 1                  ; keyboard scroll one line at a time
      scroll-conservatively 10000       ; scroll one line at a time if point moves off-screen
      scroll-preserve-screen-position t ; keep point at the same screen position
      scroll-margin 0                   ; set scroll margin
      auto-window-vscroll nil)          ; don't adjust window-vscroll to view tall lines

(setq x-select-enable-clipboard t) ; cut and paste to the X clipboard
(setq mouse-yank-at-point t) ; paste at point NOT at cursor

;;-------------------------
;; Backup and restore Emacs
;;-------------------------

;; Create a backup file
(setq backup-by-copying t                                    ; don't clobber symlinks
      backup-directory-alist '(("." . "~/.emacs.d/backup/")) ; don't litter my fs tree
      kept-new-versions 4                                    ; keep 4 last backups
      kept-old-versions 0                                    ; don't keep first backups
      delete-old-versions t                                  ; delete intermediate backup files
      version-control t)                                     ; use versioned backups

;; Automatically save and restore sessions
(desktop-save-mode 1)
(setq desktop-path '("~/.emacs.d/desktop/")
      desktop-save t
      desktop-load-locked-desktop nil)

;; Show recent files in menu
(recentf-mode 1)

;; If everything is OK, then a server starts
(server-start)

;;; init.el ends here
