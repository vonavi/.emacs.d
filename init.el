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

;; Mode bar preferences
(column-number-mode +1)           ; show column number in mode-line
(setq display-time-day-and-date t ; display the day and date in the mode line
      display-time-24hr-format t  ; use 24hr format
      display-time-interval 10    ; redisplay every ten seconds
      display-time-default-load-average nil) ; don't display the system load average
(display-time)

;;----------------------------
;; Configure and load packages
;;----------------------------

;; Use available package.el repositories
(add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(add-to-list 'load-path (expand-file-name "packages/" user-emacs-directory))

;; Set up package customization in a declarative way
(require 'config/use-package)

;; Built-in packages
(require 'config/auto-insert)
(require 'config/auto-revert)
(require 'config/bibtex)
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

;; Quickly find and act on bibliographic references
(require 'config/citar)
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

;; Major mode for editing CMake sources
(use-package cmake-mode)
;; An Emacs mode for handling Dockerfiles
(use-package dockerfile-mode)
;; Major mode for editing Protocol buffers
(use-package protobuf-mode)
;; Major mode for editing files in the YAML data serialization format
(use-package yaml-mode)

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
      backup-directory-alist            ; don't litter my fs tree
      `(("." . ,(expand-file-name "backup/" user-emacs-directory)))
      kept-new-versions 4           ; keep 4 last backups
      kept-old-versions 0           ; don't keep first backups
      delete-old-versions t         ; delete intermediate backup files
      version-control t)            ; use versioned backups

;; Automatically save and restore sessions
(desktop-save-mode 1)
(setq desktop-path `(,(expand-file-name "desktop/" user-emacs-directory))
      desktop-save t
      desktop-load-locked-desktop t)

;; File for storing customization information
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
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
