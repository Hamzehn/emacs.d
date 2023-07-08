;; Fill column indicator

(defun sanityinc/prog-mode-fci-settings ()
  (setq indicate-buffer-boundaries 'left)
  (setq display-fill-column-indicator-column 80)
  (set-face-foreground 'fill-column-indicator "#363C46")
  (display-fill-column-indicator-mode)
  (when show-trailing-whitespace
    (set (make-local-variable 'whitespace-style) '(face trailing))
    (whitespace-mode 1)))

(add-hook 'prog-mode-hook 'sanityinc/prog-mode-fci-settings)

(provide 'init-fci)
