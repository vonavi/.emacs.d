;;; init-auto-insert.el ---

;; Copyright (C) 2013 Vladimir S. Ivanov
;;
;; Author: Vladimir S. Ivanov <ivvl82@gmail.com>
;; Version:
;; Keywords:

;;; Code:

(require 'autoinsert)
(auto-insert-mode 1) ; adds hook to find-files-hook
(setq auto-insert-directory "~/.emacs.d/auto-insert/" ; *NOTE* Trailing slash important
      auto-insert-query nil                           ; don't prompt before insertion
      auto-insert 'other)                             ; insert if possible, but mark as unmodified

(setq auto-insert-alist
      '(("\\.el\\'" . ["insert.el" auto-update-template])
        ("\\.c\\'" . ["insert.c" auto-update-template])
        ("\\.h\\'" . ["insert.h" auto-update-template])))

(setq auto-update-template-alist
      '(
        ("(>>AUTHOR<<)" . "Vladimir S. Ivanov <ivvl82@gmail.com>")
        ("(>>USER_NAME<<)" . "Vladimir S. Ivanov")
        (file . (file-name-nondirectory buffer-file-name))
        ("(>>FILE<<)" . file)
        ("(>>FILE_SANS<<)" . (file-name-sans-extension file))
        ("(>>FILE_UPCASE<<)" . (subst-char-in-string ?- ?_
                                (upcase (file-name-sans-extension file))))
        ("(>>YEAR<<)" . (format-time-string "%Y" (current-time)))
        ))

(defun auto-update-template ()
  (let ((case-fold-search nil)
        local-vars)
    (dolist (template auto-update-template-alist)
      (let ((key (car template))
            (value (cdr template)))
        (if (symbolp key)
            (unless (boundp key)
              (set (make-local-variable key) (eval value))
              (push key local-vars))
          (save-excursion
            (while (re-search-forward key nil t)
              (replace-match (eval value) t))))))
    (mapc 'kill-local-variable local-vars)))

;;; init-auto-insert.el ends here
