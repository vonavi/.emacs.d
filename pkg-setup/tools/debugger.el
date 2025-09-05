;;; tools/debugger.el

;;
;;; Debug Adapter Protocol for Emacs

(use-package dape
  :custom
  ;; Info buffers to the right
  (dape-buffer-window-arrangement 'right)
  ;; Showing inlay hints
  (dape-inlay-hints t)
  ;; Project users
  (dape-cwd-function (lambda () (project-root (project-current t))))
  :hook
  ;; Pulse source line (performance hit)
  (dape-display-source . pulse-momentary-highlight-one-line)
  ;; Save buffers on startup, useful for interpreted languages
  (dape-start . (lambda () (save-some-buffers t t)))
  ;; Kill compile buffer on build success
  (dape-compile . kill-buffer)
  :config
  ;; Turn on global bindings for setting breakpoints with mouse
  (dape-breakpoint-global-mode +1))

(provide 'tools/debugger)
;;; tools/debugger.el ends here
