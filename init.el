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

;;----------------------------
;; Configure and load packages
;;----------------------------

;; Use available package.el repositories
(require 'package)
(add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(add-to-list 'load-path (expand-file-name "packages/" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "pkg-setup/" user-emacs-directory))

;; Set up package customization in a declarative way
(require 'config/use-package)

(require 'checkers/spell)
(require 'completion/vertico)
(require 'emacs/dired)
(require 'emacs/undo)
(require 'tools/direnv)
(require 'tools/magit)
(require 'tools/tree-sitter)

;; Built-in packages
(require 'config/auto-insert)
(require 'config/auto-revert)
(require 'config/desktop)
(require 'config/display-fill-column-indicator)
(require 'config/display-line-numbers)
(require 'config/eglot)
(require 'config/files)
(require 'config/mouse)
(require 'config/server)
(require 'config/so-long)
(require 'config/time)

;; Smart hungry deletion of whitespace
(require 'config/smart-hungry-delete)
;; Unobtrusively remove trailing whitespace
(require 'config/ws-butler)

;; A BibTeX backend for completion frameworks
(require 'config/bibtex-completion)
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
;; Tree-sitter support for C and C++
(require 'config/c-ts-mode)
;; A set of Emacs functions and bindings to google under point
(require 'config/google-this)
;; A major mode for editing Markdown-formatted text files
(require 'config/markdown)
;; Provide rich support for citations, labels and cross-references in Org-mode
(require 'config/org-ref)
;; Implementation of Pomodoro and Third Time techniques
(require 'config/pomm)
;; Generic Emacs interface for interactive proof assistants
(require 'config/proof-general)
;; Python's flying circus support for Emacs
(require 'config/python)
;; Highlight delimiters according to their depth
(require 'config/rainbow-delimiters)
;; Interface for sdcv (StartDict console version)
(require 'config/sdcv)
;; The Solarized colour theme, ported to Emacs
(require 'config/solarized-theme)
;; Visual Fill Column
(require 'config/visual-fill-column)

;; Major mode for editing CMake sources
(use-package cmake-mode)
;; Emacs mode for handling Dockerfiles
(use-package dockerfile-mode)
;; Major mode for editing Protocol buffers
(use-package protobuf-mode)
;; Major mode for editing files in the YAML data serialization format
(use-package yaml-mode)

;;-------------------------
;; Backup and restore Emacs
;;-------------------------

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
