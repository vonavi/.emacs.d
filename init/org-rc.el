;;; org-rc.el ---

;; Copyright (C) 2013 Vladimir S. Ivanov

;; Author: Vladimir S. Ivanov <ivvl82@gmail.com>

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(defadvice org-protocol-store-link (before rstrip-link activate)
  "Force `org-protocol-store-link' to store the URL without
trailing slash \"/\" and \"/index.output_ext\". Here, \"%2F\" is
the URL-encoded symbol of slash."
  (ad-set-arg 0 (replace-regexp-in-string
                 "%2F\\(index\\.[a-zA-Z]+\\)?/" "/"
                 (ad-get-arg 0))))

;; Set Jekyll’s handler for org-protocol
(require 'org-protocol-jekyll)
(setq org-protocol-jekyll-alist
      '(("Локальная версия сайта"
         :base-url "http://localhost:4000"
         :permalink "pretty"
         :working-directory "~/Dropbox/openshift/blog"
         :working-suffix ".org")
        ("Редактор на стероидах"
         :base-url "http://www.vonavi.me"
         :permalink "pretty"
         :working-directory "~/Dropbox/openshift/blog"
         :working-suffix ".org")))

;;; org-rc.el ends here
