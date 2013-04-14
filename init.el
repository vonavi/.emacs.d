;;; init.el ---

;; Copyright (C) 2013 Vladimir S. Ivanov

;; Author: Vladimir S. Ivanov <ivvl82@gmail.com>

;;; Commentary:

;;; Code:

;;---------------------------
;; Emacs Lisp Package Archive
;;---------------------------

(load-file "~/.emacs.d/elpa/package.el")
;; Use all available package repositories
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
;; Refresh the packages descriptions
(package-refresh-contents)
;; List of packages to load
(setq package-load-list '(all))
;; Initialize & Install Packages
(package-initialize)

(defvar prelude-packages
  '(hungry-delete)
  "A list of packages to ensure are installed at launch.")
;; Make sure the prelude packages are installed
(dolist (p prelude-packages)
  (unless (package-installed-p p)
    (package-install p)))

;;-----------------------
;; Emacs appearance setup
;;-----------------------

(add-to-list 'default-frame-alist '(font . "Consolas 16")) ; set default font
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
      setscroll-step 1            ; keyboard scroll one line at a time
      scroll-conservatively 10000 ; scroll one line at a time if point moves off-screen
      scroll-preserve-screen-position t ; keep point at the same screen position
      scroll-margin 0                   ; set scroll margin
      auto-window-vscroll nil) ; don't adjust window-vscroll to view tall lines

(setq x-select-enable-clipboard t)  ; cut and paste to the X clipboard
(setq mouse-yank-at-point t)        ; paste at point NOT at cursor

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
      desktop-load-locked-desktop nil)

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
(setq custom-file "~/.emacs.d/init/init-custom.el")

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

;; Automatically fill lines as text is inserted
(setq-default auto-fill-function 'do-auto-fill)
(setq comment-auto-fill-only-comments t) ; auto fill comments but not code
;; Auto fill not only inside comments for LaTeX files
(add-hook 'LaTeX-mode-hook
          (lambda ()
            (set (make-local-variable 'comment-auto-fill-only-comments) nil)))

;; Indentation setup
(setq-default indent-tabs-mode nil) ; never use tab characters for indentation
(setq tab-width 4                   ; set tab-width
      c-basic-offset 2              ; indentation level in CC mode
      js-indent-level 2             ; indentation level in JS mode
      css-indent-offset 2)          ; indentation level in CSS mode

;; A single <DEL> or <BS> command deletes a contiguous block of whitespace
(require 'hungry-delete)
(global-hungry-delete-mode 1)
;; Need to remap <DEL> key to `hungry-delete-forward' for Emacs 24
(define-key hungry-delete-mode-map [remap delete-forward-char] 'hungry-delete-forward)

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

;;--------------------------------------
;; Load the rest of initialization files
;;--------------------------------------

(let ((init-dir "~/.emacs.d/init/"))
  (dolist (file (directory-files init-dir))
    (when
        ;; Load Emacs Lisp source code but hidden files
        (string-match-p "\\`[^.].*\\.elc?\\'" file)
      (load-file (expand-file-name file init-dir)))))

;;; init.el ends here
