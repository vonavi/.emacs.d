;;; init.el ---

;; Copyright (C) 2013-2021  Vladimir Ivanov

;; Author: Vladimir Ivanov <ivvl82@gmail.com>
;; Keywords:

;;; Commentary:

;;

;;; Code:

;;-------
;; El-Get
;;-------

;; Ensure that El-Get is available
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

;; Use available package.el repositories
(setq package-archives
      '(("elpa" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")))

;; Build the El-Get copy of ELPA and EmacsWiki packages if we have not
;; built them before
(unless (file-directory-p el-get-recipe-path-elpa)
  (el-get-elpa-build-local-recipes))
(unless (file-directory-p el-get-recipe-path-emacswiki)
  (el-get-emacswiki-build-local-recipes))

;; Look for init-pkgname.el configurations here
(setq el-get-user-package-directory
      (concat user-emacs-directory "init-files"))

(el-get-save-package-status 'el-get "installed")
(el-get-do-init 'el-get)

;;-----------------------
;; Emacs appearance setup
;;-----------------------

(add-to-list 'default-frame-alist '(font . "PragmataPro 14")) ; set default font
(blink-cursor-mode 0)            ; no blinking cursor
(setq inhibit-splash-screen t)   ; prevent silly initial splash screen
(tool-bar-mode 0)                ; no tool bar
(set-scroll-bar-mode 'right)     ; place scroll bar

;; Create a reasonable title bar
(add-hook 'window-configuration-change-hook
          (lambda ()
            (setq frame-title-format
                  (concat
                   invocation-name "@" system-name ": "
                   (replace-regexp-in-string
                    (getenv "HOME") "~" (or buffer-file-name "%b"))))))

;; Mode bar preferences
(column-number-mode 1)            ; show column number in mode-line
(setq display-time-day-and-date t ; display the day and date in the mode line
      display-time-24hr-format t  ; use 24hr format
      display-time-interval 10    ; redisplay every ten seconds
      display-time-default-load-average nil) ; don't display the system load average
(display-time)

;;-----------------
;; Emacs feel setup
;;-----------------

;; Enable mouse wheel
(mouse-wheel-mode 1)
(setq mouse-wheel-scroll-amount '(1) ; mouse scroll one line at a time
      mouse-wheel-progressive-speed nil ; don't accelerate scrolling
      mouse-wheel-follow-mouse t        ; scroll window under mouse
      scroll-conservatively 10000) ; scroll one line at a time if point moves off-screen

(setq x-select-enable-clipboard t   ; cut and paste to the X clipboard
      mouse-yank-at-point t)        ; paste at point NOT at cursor

;;-------------------------
;; Backup and restore Emacs
;;-------------------------

;; Create a backup file
(setq backup-by-copying t               ; don't clobber symlinks
      backup-directory-alist '(("." . "~/.emacs.d/backup/")) ; don't litter my fs tree
      kept-new-versions 4           ; keep 4 last backups
      kept-old-versions 0           ; don't keep first backups
      delete-old-versions t         ; delete intermediate backup files
      version-control t)            ; use versioned backups

;; Automatically save and restore sessions
(desktop-save-mode 1)
(setq desktop-path '("~/.emacs.d/desktop/")
      desktop-save t
      desktop-load-locked-desktop t)

;; File for storing customization information
(setq custom-file "~/.emacs.d/init-files/init-custom.el")

;;---------------------
;; Emacs buffer editing
;;---------------------

(setq sentence-end-double-space nil     ; sentences end with one space
      require-final-newline t)          ; always end a file with a newline

;; Enable Visual Line mode
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
(setq visual-line-fringe-indicators '(nil right-curly-arrow))

;; Avoid performance issues in files with very long lines
(global-so-long-mode 1)

;; Indentation setup
(electric-indent-mode 1)            ; auto indentation
(setq-default indent-tabs-mode nil) ; never use tab characters for indentation
(setq tab-width 4                   ; set tab-width
      c-default-style "stroustrup"  ; indent style in CC mode
      js-indent-level 2             ; indentation level in JS mode
      css-indent-offset 2)          ; indentation level in CSS mode

;;--------------------
;; Miscellaneous stuff
;;--------------------

(setq default-input-method "russian-computer") ; default input method
(show-paren-mode 1)                      ; highlight parenthesis pairs
(setq blink-matching-paren-distance nil) ; search backwards for matching open-paren
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on) ; translate escape sequences
(setq calendar-week-start-day 1         ; week starts Monday
      calendar-date-style 'european)    ; European style calendar
(fset 'yes-or-no-p 'y-or-n-p)       ; use y or n instead of yes or not

;; This function reverts all buffers that are visiting a file.
(defun revert-all-buffers ()
  "Refreshes all open buffers from their respective files."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name) (not (buffer-modified-p)))
        (revert-buffer t t t))))
  (message "Refreshed open files."))

;;; init.el ends here
