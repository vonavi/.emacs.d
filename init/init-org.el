;;; init-org.el ---

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

;; Set Jekyll’s handler for org-protocol
(require 'org-protocol-jekyll)
(setq org-protocol-jekyll-alist
      '(("Локальная версия сайта"
         :base-url "http://localhost:4000"
         :permalink "pretty"
         :working-directory "~/Dropbox/openshift/blog/source"
         :working-suffix ".org")
        ("Редактор на стероидах"
         :base-url "http://www.vonavi.me"
         :permalink "pretty"
         :working-directory "~/Dropbox/openshift/blog/source"
         :working-suffix ".org")))

;;; init-org.el ends here
