;; Fill Column Mode Changes
;; Most of the code below from GitHub user Purcell
;; https://github.com/purcell/emacs.d/blob/master/lisp/init-fci.el

(require-package 'fill-column-indicator)

(defun sanityinc/prog-mode-fci-settings ()
  (setq fci-rule-column 79)
  (setq fci-rule-color "#363C46")
  ;;  (setq fci-rule-character ?|)
  (turn-on-fci-mode)
  (when show-trailing-whitespace
    (set (make-local-variable 'whitespace-style) '(face trailing))
    (whitespace-mode 1))
  )

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

;; Regenerate fci-mode line images after switching themes
;; (defadvice enable-theme (after recompute-fci-face activate)
;;   (dolist (buffer (buffer-list))
;;     (with-current-buffer buffer
;;       (when (sanityinc/fci-enabled-p)
;;         (turn-on-fci-mode)))))

(add-hook 'prog-mode-hook 'sanityinc/prog-mode-fci-settings)

;; Do the same thing as above for company mode using hooks

(defvar-local hn/company-fci-mode-on-p nil)

(defun hn/company-turn-off-fci (&rest ignore)
  (when (boundp 'fci-mode)
    (setq hn/company-fci-mode-on-p fci-mode)
    (when fci-mode (fci-mode -1))))

(defun hn/company-maybe-turn-on-fci (&rest ignore)
  (when hn/company-fci-mode-on-p (fci-mode 1)))

(add-hook 'company-completion-started-hook 'hn/company-turn-off-fci)
(add-hook 'company-completion-finished-hook 'hn/company-maybe-turn-on-fci)
(add-hook 'company-completion-cancelled-hook 'hn/company-maybe-turn-on-fci)

;; Themes

;; Noctilux theme
;;(require-package 'noctilux-theme)
;;(load-theme 'noctilux t)

;; Zenburn theme
;;(require-package 'zenburn-theme)
;;(load-theme 'zenburn t)
;; High Contrast Zenburn theme version
;;(require-package 'hc-zenburn-theme)
;; (load-theme 'hc-zenburn t)

;; Ample Zen Theme
;;(require-package 'ample-zen-theme)
;; (load-theme 'ample-zen t)

;; Material Theme
;;(require-package 'material-theme)
;; (load-theme 'material t)

;; Atom One Dark Theme
(require-package 'atom-one-dark-theme)
(load-theme 'atom-one-dark)

;; show the menu bar
(menu-bar-mode 1)

;; Setup frame size and windows
(add-to-list 'default-frame-alist '(height . 56))
(add-to-list 'default-frame-alist '(width . 192))

;; Set the font
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "outline" :slant normal :weight normal :height 100 :width normal)))))

;; Highlight current line
(global-hl-line-mode 1)

;; Set global line number mode
(global-linum-mode 1)
(defadvice linum-update-window (around linum-dynamic activate)
  (let* ((w (length (number-to-string
                     (count-lines (point-min) (point-max)))))
         (linum-format (concat "%" (number-to-string w) "d ")))
    ad-do-it))

(provide 'init-local)
