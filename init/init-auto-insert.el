;;; init-auto-insert.el ---

;; Copyright (C) 2013 Vladimir S. Ivanov

;; Author: Vladimir S. Ivanov <ivvl82@gmail.com>

;;; Commentary:

;;; Code:

(require 'autoinsert)
(auto-insert-mode 1)                    ; adds hook to find-files-hook
(setq auto-insert-directory "~/.emacs.d/auto-insert/" ; *NOTE* Trailing slash important
      auto-insert-query nil            ; don't prompt before insertion
      auto-insert 'other) ; insert if possible, but mark as unmodified

(setq auto-insert-alist
      '(("\\.el\\'" . ["insert.el" my-auto-insert-template])
        ("\\.c\\'" . ["insert.c" my-auto-insert-template])
        ("\\.h\\'" . ["insert.h" my-auto-insert-template])))

(defvar my-auto-insert-alist
  '(("(>>AUTHOR<<)" . "Vladimir S. Ivanov <ivvl82@gmail.com>")
    ("(>>USER_NAME<<)" . "Vladimir S. Ivanov")
    (file . (file-name-nondirectory buffer-file-name))
    (file-sans-ext . (file-name-sans-extension file))
    ("(>>FILE<<)" . file)
    ("(>>FILE_SANS<<)" . file-sans-ext)
    ("(>>FILE_UPCASE<<)" . (subst-char-in-string ?- ?_ (upcase file-sans-ext)))
    ("(>>YEAR<<)" . (format-time-string "%Y" (current-time))))
  "An association list of templates to be inserted into a new file.")

(defun my-auto-insert-template ()
  "Insert templates into a new file in accordance with `my-auto-insert-alist'."
  (let ((case-fold-search nil)
        local-vars)
    (dolist (template my-auto-insert-alist)
      (let ((key (car template))
            (value (eval (cdr template))))
        (cond
         ((stringp key)
          (save-excursion
            (while (re-search-forward key nil t)
              (replace-match value t))))
         ((symbolp key)
          (unless (boundp key)
            (set (make-local-variable key) value)
            (push key local-vars))))))
    (mapc 'kill-local-variable local-vars)))

;;; init-auto-insert.el ends here
