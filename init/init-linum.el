;;; init-linum.el ---

;; Copyright (C) 2013 Vladimir S. Ivanov

;; Author: Vladimir S. Ivanov <ivvl82@gmail.com>

;;; Commentary:

;;; Code:

(require 'linum)

;; Makes line numbers in linum-mode to be right-aligned and one-space
;; separated from the buffer contents

(add-hook 'linum-before-numbering-hook 'linum-get-format-string)

(defun linum-get-format-string ()
  "Get the string format of line numbers limited by window.
Use the maximal point LIMIT of the window available within
`linum-update-window'."
  (let* ((width (length (number-to-string
                         (1+ (count-lines (point-min) limit)))))
         (format (concat "%" (number-to-string width) "d")))
    (setq linum-format-string format)))

(setq linum-format (lambda (line-number)
                     (propertize (format linum-format-string line-number)
                                 'face 'linum)))

;; Provides an interface for turning line-numbering off

;; Enabled `global-linum-mode' causes that magit hangs after
;; log-edit-commit. Disabling `linum-mode' in the magit buffer status
;; solved the problem. See at
;; `https://github.com/magit/magit/issues/494'.

(setq linum-disabled-buffers-list '("\\` " "\\`\\*")
      linum-disabled-modes-list '(dired-mode fundamental-mode latex-mode
                                             text-mode wl-summary-mode))

(defadvice linum-on (around linum-on-around activate)
  "When linum is running globally, disable line number in buffers
defined in `linum-disabled-buffers-list' and modes defined in
`linum-disabled-modes-list'."
  (unless (or (some (lambda (re) (string-match-p re (buffer-name)))
                    linum-disabled-buffers-list)
              (apply 'derived-mode-p linum-disabled-modes-list))
    ad-do-it))

;; Enable linum-mode
(global-linum-mode)

;;; init-linum.el ends here
