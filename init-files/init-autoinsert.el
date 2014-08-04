;;; init-autoinsert.el ---

;; Copyright (C) 2014  Vladimir Ivanov

;; Author: Vladimir Ivanov <ivvl82@gmail.com>
;; Keywords:

;;; Commentary:

;;

;;; Code:

(require 'autoinsert)
(auto-insert-mode 1)                   ; adds hook to find-files-hook
(setq auto-insert-query nil            ; don't prompt before insertion
      auto-insert 'other) ; insert if possible, but mark as unmodified

;; User information to be auto-inserted
(setq user-full-name "Vladimir Ivanov"
      user-mail-address "ivvl82@gmail.com")

;;; init-autoinsert.el ends here
