;;; auto-insert.el ---

;; Copyright (C) 2024  Vladimir Ivanov

;; Author: Vladimir Ivanov <ivvl82@gmail.com>
;; Keywords:

;;; Commentary:

;;

;;; Code:

(eval-when-compile
  (require 'subr-x))

(use-package autoinsert
  :init
  (auto-insert-mode +1)

  (setq auto-insert-query nil          ; don't prompt before insertion
        auto-insert 'other) ; insert if possible, but mark as unmodified

  ;; User information to be auto-inserted
  (setq user-full-name
        (string-trim-right (shell-command-to-string "git config user.name"))
        user-mail-address
        (string-trim-right (shell-command-to-string "git config user.email"))))

(provide 'config/auto-insert)
;;; auto-insert.el ends here
