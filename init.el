;;; init.el ---

;; Copyright (C) 2013-2021  Vladimir Ivanov

;; Author: Vladimir Ivanov <ivvl82@gmail.com>
;; Keywords:

;;; Commentary:

;;

;;; Code:

;;-----------------------
;; Emacs appearance setup
;;-----------------------

(set-face-attribute 'default nil
                    :family "JetBrainsMono Nerd Font"
                    :weight 'normal
                    :height 120) ; set the default font
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
(column-number-mode +1)           ; show column number in mode-line
(setq display-time-day-and-date t ; display the day and date in the mode line
      display-time-24hr-format t  ; use 24hr format
      display-time-interval 10    ; redisplay every ten seconds
      display-time-default-load-average nil) ; don't display the system load average
(display-time)

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
(add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; Build the El-Get copy of ELPA and EmacsWiki packages if we have not
;; built them before
(unless (file-directory-p el-get-recipe-path-elpa)
  (el-get-elpa-build-local-recipes))
(unless (file-directory-p el-get-recipe-path-emacswiki)
  (el-get-emacswiki-build-local-recipes))

;; Look for init-pkgname.el configurations here
(setq el-get-user-package-directory
      (concat user-emacs-directory "init-files/"))

(el-get-save-package-status 'el-get "installed")
(el-get-do-init 'el-get)

;;----------------------------
;; Configure and load packages
;;----------------------------

;; Store BibTeX files here
(defvar my/bib-files
  `(,(expand-file-name "~/org/bibliography/references.bib")))
;; Store PDFs here
(defvar my/pdf-dirs
  `(,(expand-file-name "~/org/bibliography/bibtex-pdfs/")))

;; Set up the load-paths and autoloads for installed packages
(package-initialize)

(add-to-list 'load-path (concat user-emacs-directory "packages/"))

;; Set up package customization in a declarative way
(require 'config/use-package)

;; Built-in packages
(require 'config/auto-insert)
(require 'config/auto-revert)
(require 'config/dired)
(require 'config/display-fill-column-indicator)
(require 'config/flyspell)
(require 'config/server)
(require 'config/so-long)

;; Show line numbers in the margin
(require 'config/nlinum)
;; Visual Fill Column
(require 'config/visual-fill-column)

;; Smart hungry deletion of whitespace
(require 'config/smart-hungry-delete)
;; Unobtrusively remove trailing whitespace
(require 'config/ws-butler)

;; Org Mode is a mode for document editing, formatting, and organizing
(require 'config/org)
;; Rudimentary Roam replica with Org-mode
(require 'config/org-roam)

;; Extensible package for writing and formatting TeX files
(require 'config/auctex)
;; Jump to things in Emacs tree-style
(require 'config/avy)
;; A set of Emacs functions and bindings to google under point
(require 'config/google-this)
;; Complete text-based user interface to Git
(require 'config/magit)
;; A major mode for editing Markdown-formatted text files
(require 'config/markdown)
;; Provide rich support for citations, labels and cross-references in Org-mode
(require 'config/org-ref)
;; Implementation of Pomodoro and Third Time techniques
(require 'config/pomm)
;; Generic Emacs interface for interactive proof assistants
(require 'config/proof-general)
;; Highlight delimiters according to their depth
(require 'config/rainbow-delimiters)
;; Interface for sdcv (StartDict console version)
(require 'config/sdcv)
;; The Solarized colour theme, ported to Emacs
(require 'config/solarized-theme)
;; Treat undo history as a tree
(require 'config/undo-tree)
;; VERTical Interactive COmpletion
(require 'config/vertico)

;; An Emacs mode for handling Dockerfiles
(use-package dockerfile-mode)
;; Major mode for editing Protocol buffers
(use-package protobuf-mode)

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
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;;---------------------
;; Emacs buffer editing
;;---------------------

(setq sentence-end-double-space nil     ; sentences end with one space
      require-final-newline t)          ; always end a file with a newline

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
