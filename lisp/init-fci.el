;; Fill column indicator
(require-package 'fill-column-indicator)
(defun sanityinc/prog-mode-fci-settings ()
  (setq fci-rule-column 79)
  (setq fci-rule-color "#363C46")
  (turn-on-fci-mode)
  (when show-trailing-whitespace
    (set (make-local-variable 'whitespace-style) '(face trailing))
    (whitespace-mode 1)))

(defun sanityinc/fci-enabled-p ()
  (bound-and-true-p fci-mode))

(defvar sanityinc/fci-mode-suppressed nil)
(make-variable-buffer-local 'sanityinc/fci-mode-suppressed)

(defadvice popup-create (before suppress-fci-mode activate)
  "Suspend fci-mode while popups are visible"
  (let ((fci-enabled (sanityinc/fci-enabled-p)))
    (when fci-enabled
      (setq sanityinc/fci-mode-suppressed fci-enabled)
      (turn-off-fci-mode))))
(defadvice popup-delete (after restore-fci-mode activate)
  "Restore fci-mode when all popups have closed"
  (when (and sanityinc/fci-mode-suppressed
             (null popup-instances))
    (setq sanityinc/fci-mode-suppressed nil)
    (turn-on-fci-mode)))

(add-hook 'prog-mode-hook 'sanityinc/prog-mode-fci-settings)

;; Do the same thing as above for company mode using hooks

(defvar-local sanityinc/company-fci-mode-on-p nil)

(defun sanityinc/company-turn-off-fci (&rest ignore)
  (when (boundp 'fci-mode)
    (setq sanityinc/company-fci-mode-on-p fci-mode)
    (when fci-mode (fci-mode -1))))

(defun sanityinc/company-maybe-turn-on-fci (&rest ignore)
  (when sanityinc/company-fci-mode-on-p (fci-mode 1)))

(add-hook 'company-completion-started-hook 'sanityinc/company-turn-off-fci)
(add-hook 'company-completion-finished-hook 'sanityinc/company-maybe-turn-on-fci)
(add-hook 'company-completion-cancelled-hook 'sanityinc/company-maybe-turn-on-fci)


(provide 'init-fci)
