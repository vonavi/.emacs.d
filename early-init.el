;;; early-init.el

;; Enable pixelwise resizing of frames and windows
(setq frame-resize-pixelwise t
      window-resize-pixelwise t)
;; Don't resize frame to a specific column size
(setq frame-inhibit-implied-resize t)

;; Prevent silly initial splash screen
(setq inhibit-startup-screen t)
;; Hide the tool bar on all frames
(tool-bar-mode -1)
;; Specify whether to have vertical scroll bars and where to place them
(set-scroll-bar-mode 'right)

;; Set the default font
(set-face-attribute 'default nil
                    :family "JetBrainsMono Nerd Font"
                    :weight 'normal
                    :height 120)

;; Create a reasonable title bar
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))
        (:eval (when (buffer-modified-p)
                 " *"))))

;; Disable blinking cursor
(blink-cursor-mode -1)
;; Disable the annoying bell ring
(setq ring-bell-function #'ignore)

;;; early-init.el ends here
