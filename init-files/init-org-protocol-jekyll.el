;;; init-org-protocol-jekyll.el ---

;; Copyright (C) 2014  Vladimir Ivanov

;; Author: Vladimir Ivanov <ivvl82@gmail.com>
;; Keywords:

;;; Commentary:

;;

;;; Code:

;; Set Jekyll's handler for org-protocol
(require 'org-protocol-jekyll)
(setq org-protocol-jekyll-alist
      '(("Локальная версия сайта"
         :base-url "http://localhost:4000"
         :permalink "pretty"
         :working-directory "~/openshift/jekyll"
         :working-suffix ".org")
        ("Редактор на стероидах"
         :base-url "http://www.vonavi.me"
         :permalink "pretty"
         :working-directory "~/openshift/jekyll"
         :working-suffix ".org")))

;;; init-org-protocol-jekyll.el ends here
