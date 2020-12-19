;;; init-themes.el --- Defaults for themes -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'doom-themes)

;; If you don't customize it, this is the theme you get.
(setq-default custom-enabled-themes '(doom-vibrant))

;; Ensure that themes will be applied even if they have not been customized
(defun reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme t)))
  (custom-set-variables `(custom-enabled-themes
                          (quote ,custom-enabled-themes))))

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

;; brighten buffers (that represent real files)
(require-package 'solaire-mode)
(add-hook 'after-change-major-mode-hook #'turn-on-solaire-mode)

;; To enable solaire-mode unconditionally for certain modes:
(add-hook 'ediff-prepare-buffer-hook #'solaire-mode)

;; ...if you use auto-revert-mode:
(add-hook 'after-revert-hook #'turn-on-solaire-mode)

;; highlight the minibuffer when it is activated:
(add-hook 'minibuffer-setup-hook #'solaire-mode-in-minibuffer)

;; if the bright and dark background colors are the wrong way around, use this
;; to switch the backgrounds of the `default` and `solaire-default-face` faces.
;; This should be used *after* you load the active theme!

;;-----------------------------------------------------------------------------
;; Toggle between light and dark
;;-----------------------------------------------------------------------------
(defun light ()
  "Activate a light color theme."
  (interactive)
  (setq custom-enabled-themes '(doom-one-light))
  (reapply-themes)
  (solaire-mode-swap-bg))

(defun dark ()
  "Activate a dark color theme."
  (interactive)
  (setq custom-enabled-themes '(doom-vibrant))
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
