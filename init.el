;;; init.el ---

;; Copyright (C) 2013-2015  Vladimir Ivanov

;; Author: Vladimir Ivanov <ivvl82@gmail.com>
;; Keywords:

;;; Commentary:

;;

;;; Code:

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;;-------
;; El-Get
;;-------

;; Ensure that El-Get is available
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://github.com/dimitri/el-get/raw/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

;; Use available package.el repositories
(add-to-list 'package-archives
             '("elpa" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives
             '("marmalade" . "https://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

;; Build the El-Get copy of the package.el packages if we have not
;; built it before
(unless (file-directory-p el-get-recipe-path-elpa)
  (el-get-elpa-build-local-recipes))

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

;; List of most recent files, persisent between emacs session.
(recentf-mode 1)
(setq recentf-save-file "~/.emacs.d/cache/.recentf"
      recentf-exclude '("/\\.[^/]+\\'" "/\\.emacs\\.d/elpa/" "/loaddefs\\.el\\'"))
;; Get ido to handle recentf results
(defun ido-recentf-open-files ()
  "Use ido to select a recently opened file from the `recentf-list'"
  (interactive)
  (find-file (ido-completing-read
              "Find recent file: "
              (mapcar (apply-partially
                       'replace-regexp-in-string (getenv "HOME") "~")
                      recentf-list))))

;; File for storing customization information
(setq custom-file "~/.emacs.d/init-files/init-custom.el")

;;--------------------------
;; Emacs minibuffer behavior
;;--------------------------

;; Do interactively things with buffers and files
(ido-mode 1)
(setq ido-save-directory-list-file "~/.emacs.d/cache/ido.last"
      ido-ignore-buffers '("\\` " "\\`\\*") ; ignore these buffers
      ido-confirm-unique-completion t ; wait for RET with unique completion
      confirm-nonexistent-file-or-buffer nil) ; don't need confirmation

(icomplete-mode 1)           ; completion in mini-buffer
(setq resize-mini-windows t) ; minibuffer gets resized if it becomes too big
(fset 'yes-or-no-p 'y-or-n-p)       ; use y or n instead of yes or not

;;---------------------
;; Emacs buffer editing
;;---------------------

;; Disable displaying Bi-directional text
(setq-default bidi-display-reordering nil)
(setq sentence-end-double-space nil ; sentences end with one space
      require-final-newline t)      ; always end a file with a newline
;; Delete trailing whitespaces before saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; Stop Emacs from arbitrarily adding lines to the end of a file when
;; the cursor is moved past the end of it:
(setq next-line-add-newlines nil)

;; Enable Visual Line mode
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
(setq visual-line-fringe-indicators '(nil right-curly-arrow))

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
