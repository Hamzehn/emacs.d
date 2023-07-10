;;; init-themes.el --- Defaults for themes -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; Solaire mode must be activated before loading themes
(when (maybe-require-package 'solaire-mode)
  (solaire-global-mode +1)
  (add-hook 'after-revert-hook #'turn-on-solaire-mode)
  (add-hook 'ediff-prepare-buffer-hook #'turn-on-solaire-mode)

  ;; Turn off Solaire mode in EIN Notebooks
  ;; (it doesn't treat EIN notebook cells as real code areas)
  (when (boundp 'ein:notebook-mode-hook)
    (add-hook 'ein:notebook-mode-hook #'turn-off-solaire-mode)
    (add-hook 'ein:notebook-mode-hook
              (lambda ()
                (face-remap-add-relative 'default :background "#2A2E38")))
    (set-face-attribute 'ein:cell-input-area nil
                        :extend t
                        :background "#2A2E38")
    (set-face-attribute 'ein:cell-code-input-area nil
                        :inherit ein:cell-input-area
                        :background "#242730")
    (set-face-attribute 'ein:cell-code-input-prompt nil
                        :inherit font-lock-builtin-face
                        :underline t
                        :height 0.8)
    (set-face-attribute 'ein:cell-markdown-input-prompt nil
                        :inherit ein:markdown-markup-face
                        :underline t
                        :height 0.7)))

(when (maybe-require-package 'doom-themes)
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (setq doom-vibrant-brighter-comments nil
        doom-vibrant-brighter-modeline t
        doom-vibrant-padded-modeline nil)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

;; Don't prompt to confirm theme safety. This avoids problems with
;; first-time startup on Emacs > 26.3.
(setq custom-safe-themes t)

(defvar my/dark-theme 'doom-vibrant)
(defvar my/light-theme 'doom-solarized-light)
(defvar my/default-theme my/dark-theme)

;; If you don't customize it, this is the theme you get.
(setq-default custom-enabled-themes (list my/default-theme))

;; Ensure that themes will be applied even if they have not been customized
(defun reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme)))
  (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))

(add-hook 'after-init-hook 'reapply-themes)



;; Toggle between light and dark

(defun light ()
  "Activate a light color theme."
  (interactive)
  (disable-theme my/dark-theme) ; to force restore unspecified face attributes
  (setq custom-enabled-themes (list my/light-theme))
  (reapply-themes)
  (set-mouse-color "dim grey"))

(defun dark ()
  "Activate a dark color theme."
  (interactive)
  (disable-theme my/light-theme) ; to force restore unspecified face attributes
  (setq custom-enabled-themes (list my/dark-theme))
  (reapply-themes)
  (set-mouse-color "dark grey"))

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
