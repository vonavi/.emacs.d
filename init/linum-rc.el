;;; linum-rc.el ---

;; Copyright (C) 2013 Vladimir S. Ivanov

;; Author: Vladimir S. Ivanov <ivvl82@gmail.com>

;;; Commentary:

;;; Code:

(require 'linum)

;; Makes line numbers in linum-mode to be right-aligned and one-space
;; separated from the buffer contents

(defadvice linum-update-window (before my-linum-set-format activate)
  "Set the string format `linum-format' of line numbers for the
window. In contrast to `linum-format' of dynamic type, not all
lines in the buffer are taken into account, but visual lines
only."
  (let* ((win (ad-get-arg 0))
         (limit (window-end win t))
         (width (length (number-to-string
                         (1+ (count-lines (point-min) limit)))))
         (format (concat "%" (number-to-string width) "d")))
    (setq linum-format format)))

;; Provides an interface for turning line-numbering off

;; Enabled `global-linum-mode' causes that magit hangs after
;; log-edit-commit. Disabling `linum-mode' in the magit buffer status
;; solved the problem. See at
;; `https://github.com/magit/magit/issues/494'.

(defadvice linum-on (around my-linum-on activate)
  "When linum is running globally, enable line numbers for
programming modes only."
  (when (derived-mode-p 'prog-mode) ad-do-it))

;; Enable linum-mode
(global-linum-mode)

;;; linum-rc.el ends here
