;;; init-magit.el ---

;; Copyright (C) 2013 Vladimir S. Ivanov

;; Author: Vladimir S. Ivanov <ivvl82@gmail.com>

;;; Commentary:

;;; Code:

(require 'ansi-color)

(defun my-magit-make-color-map ()
  "Provide ANSI color map to highlight a diff in Magit mode."
  (let ((ansi-color-faces-vector
         [default default default italic underline
           bold bold-italic my-magit-diff-highlight])
        (ansi-color-names-vector
         (vector "black" (face-attribute 'magit-diff-del :foreground)
                 (face-attribute 'magit-diff-add :foreground)
                 "yellow" "blue" "magenta"
                 (face-attribute 'my-magit-range-info :foreground)
                 "white")))
    (set (make-local-variable 'ansi-color-map)
         (ansi-color-make-color-map))))

(require 'magit)

(add-hook 'magit-mode-hook 'my-magit-make-color-map)
(add-hook 'magit-mode-hook (lambda () (setq truncate-lines nil)))
;; Enable diff highlighting
(setq magit-diff-options '("--color"))

(defface my-magit-range-info
  '((t :inherit diff-context))
  "Face to highlight range information for a diff."
  :group 'magit-faces)

(defface my-magit-diff-highlight
  '((t))
  "Face to highlight changes in lines for a diff."
  :group 'magit-faces)

;; Change Magit colors for diff highlighting
(set-face-foreground 'magit-diff-del "red3")
(set-face-foreground 'magit-diff-add "green3")
(set-face-foreground 'my-magit-range-info "cyan3")
(set-face-inverse-video-p 'my-magit-diff-highlight t)

(defmacro my-magit-diff-wrap (output)
  "Check capabilities of highlighting and prepare output of a
diff. Put diff-highlight script from source
`https://github.com/git/git/tree/master/contrib/diff-highlight'
in your $PATH and make it executable to get advanced
highlighting."
  (let* ((cmd "diff-highlight")
         (file (locate-file cmd exec-path)))
    (if (file-executable-p file)
        `(with-temp-buffer
           (insert ,output)
           (shell-command-on-region (point-min) (point-max)
                                    ,file (current-buffer) t)
           (buffer-string))
      output)))

;; Re-write some functions from Magit to enable advanced highlighting

(defun magit-git-insert (args)
  (let ((beg (point)))
    (insert (my-magit-diff-wrap (magit-git-output args)))
    (ansi-color-apply-on-region beg (point))))

(defun magit-cmd-insert (cmd args)
  (let ((beg (point)))
    (insert (my-magit-diff-wrap (magit-cmd-output cmd args)))
    (ansi-color-apply-on-region beg (point))))

(defun magit-cmd-output (cmd args)
  (with-output-to-string
    (with-current-buffer standard-output
      (apply 'process-file
             cmd
             nil (list t nil) nil
             args))))

;;; init-magit.el ends here
