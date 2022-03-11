;;; init-org.el ---

;; Copyright (C) 2020  Vladimir Ivanov

;; Author: Vladimir Ivanov <ivvl82@gmail.com>
;; Keywords:

;;; Commentary:

;;

;;; Code:

(require 'org)
;; Default viewer for HTML files
(add-to-list 'org-file-apps
             '("\\.x?html?\\'" . (lambda (file path) (browse-url file))))
;; Default viewer for PDF files
(delete '("\\.pdf\\'" . default) org-file-apps)
(add-to-list 'org-file-apps '("\\.pdf\\'" . "evince %s"))
(add-to-list 'org-file-apps
             '("\\.pdf::\\([0-9]+\\)\\'" . "evince --page-label=%1 %s"))

;; Keyboard shortcuts
(define-key global-map (kbd "C-c l") 'org-store-link) ; store links
(define-key global-map (kbd "C-c a") 'org-agenda)     ; agenda view

;; Org Mode extension to send information to Emacs from web browsers
(require 'org-protocol)
(require 'server)
(unless (server-running-p) (server-start))

;;---------------
;; Org appearance
;;---------------

;; Set the font family for Org headings
(add-hook 'org-mode-hook
          (lambda ()
            (let ((font-family (face-attribute 'default :family)))
              (dolist (face '(org-level-1
                              org-level-2
                              org-level-3
                              org-level-4
                              org-level-5
                              org-level-6
                              org-level-7
                              org-level-8))
                (set-face-attribute face nil :family font-family)))))

;; Indent text according to outline structure
(add-hook 'org-mode-hook (lambda () (org-indent-mode 1)))

;; Display entities as UTF-8 characters
(setq org-pretty-entities t)

;;----------
;; Org Babel
;;----------

;; Active Babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((C . t)
   (dot . t)
   (python . t)))

;;-------------
;; LaTeX export
;;-------------

(require 'ox-latex)
;; To use the listings package automatically for LaTeX documents
(setq org-latex-listings 'minted)
;; List of additional LaTeX packages
(add-to-list 'org-latex-packages-alist '("" "listings"))
(add-to-list 'org-latex-packages-alist '("" "color"))
(add-to-list 'org-latex-packages-alist '("" "minted"))
(add-to-list 'org-latex-packages-alist '("" "cmap"))
(add-to-list 'org-latex-packages-alist '("english,russian" "babel"))
;; 'Listings' package options
(setq org-latex-listings-options
      '(("inputencoding" "utf8")
        ("extendedchars" "\\true")
        ("keepspaces" "true")
        ("basicstyle" "\\ttfamily")
        ("columns" "flexible")
        ("showstringspaces" "false")))
;; 'Minted' package options
(setq org-latex-minted-options '(("encoding" "utf8")))

;; Customize how Org-mode produces a PDF file
(setq org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "bibtex %b"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

;;; init-org.el ends here
