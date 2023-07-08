;;; init-themes.el --- Defaults for themes -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Solaire mode must be activated before loading themes
(require-package 'solaire-mode)
(solaire-global-mode +1)
;; Ensure solaire-mode is running in all solaire-mode buffers
;;(add-hook 'change-major-mode-hook #'turn-on-solaire-mode)

;; if you use auto-revert-mode, this prevents solaire-mode from turning
;; itself off every time Emacs reverts the file
(add-hook 'after-revert-hook #'turn-on-solaire-mode)

;; To enable solaire-mode unconditionally for certain modes:
(add-hook 'ediff-prepare-buffer-hook #'turn-on-solaire-mode)

;; Turn off Solaire mode in EIN Notebooks (it doesn't treat EIN notebook cells
;; as real code areas.
(add-hook 'ein:notebook-mode-hook #'turn-off-solaire-mode)
(add-hook 'ein:notebook-mode-hook
          (lambda ()
            (face-remap-add-relative 'default :background "#2A2E38")))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ein:cell-input-area ((t (:extend t :background "#2A2E38"))))
 '(ein:cell-code-input-area ((t (:inherit ein:cell-input-area :background "#242730"))))
 '(ein:cell-code-input-prompt ((t (:inherit font-lock-builtin-face :underline t :height 0.8))))
 '(ein:cell-markdown-input-prompt ((t (:inherit ein:markdown-markup-face :underline t :height 0.7)))))

;; Now we can load our theme (Doom)
(require-package 'doom-themes)

;; If you don't customize it, this is the theme you get.
(setq-default custom-enabled-themes '(doom-vibrant))

;; Global settings (defaults)
(setq doom-themes-enable-bold t     ; if nil, bold is universally disabled
      doom-themes-enable-italic t) ; if nil, italics is universally disabled

;; Enable brighter comments
(setq doom-one-brighter-comments nil
      doom-one-brighter-modeline t
      doom-one-padded-modeline nil)
(setq doom-one-light-brighter-comments nil
      doom-one-light-brighter-modeline t
      doom-one-light-padded-modeline nil)
(setq doom-vibrant-brighter-comments nil
      doom-vibrant-brighter-modeline t
      doom-vibrant-padded-modeline nil)

;; Enable flashing mode-line on errors
(doom-themes-visual-bell-config)

;; Corrects (and improves) org-mode's native fontification.
(doom-themes-org-config)

;;-----------------------------------------------------------------------------
;; Toggle between light and dark
;;-----------------------------------------------------------------------------
;; Ensure that themes will be applied even if they have not been customized
(defun reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme t)))
  (custom-set-variables `(custom-enabled-themes
                          (quote ,custom-enabled-themes))))

(add-hook 'after-init-hook 'reapply-themes)



;; Toggle between light and dark

(defun light ()
  "Activate a light color theme."
  (interactive)
  (setq custom-enabled-themes '(doom-one-light))
  (set-mouse-color "dark grey")
  (reapply-themes)
  (solaire-mode-swap-bg))

(defun dark ()
  "Activate a dark color theme."
  (interactive)
  (setq custom-enabled-themes '(doom-vibrant))
  (set-mouse-color "dark grey")
  (reapply-themes))

(add-hook 'after-init-hook 'dark)

(when (maybe-require-package 'dimmer)
  (setq-default dimmer-fraction 0.15)
  (add-hook 'after-init-hook 'dimmer-mode)
  (with-eval-after-load 'dimmer
    ;; TODO: file upstream as a PR
    (advice-add 'frame-set-background-mode :after (lambda (&rest args) (dimmer-process-all))))
  (with-eval-after-load 'dimmer
    ;; Don't dim in terminal windows. Even with 256 colours it can
    ;; lead to poor contrast.  Better would be to vary dimmer-fraction
    ;; according to frame type.
    (defun sanityinc/display-non-graphic-p ()
      (not (display-graphic-p)))
    (add-to-list 'dimmer-exclusion-predicates 'sanityinc/display-non-graphic-p)))


(provide 'init-themes)
;;; init-themes.el ends here
