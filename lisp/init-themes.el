(require-package 'doom-themes)
(require-package 'neotree)
(require-package 'all-the-icons)

;; If you don't customize it, this is the theme you get.
(setq-default custom-enabled-themes '(doom-vibrant))

;; Ensure that themes will be applied even if they have not been customized
(defun reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme)))
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

;; Enable custom neotree theme
(doom-themes-neotree-config)  ; all-the-icons fonts must be installed!
;; Neotree Doom Theme Customizations
(setq doom-neotree-enable-variable-pitch t
      doom-neotree-line-spacing nil
      neo-show-hidden-files t)
(set-face-attribute 'doom-neotree-dir-face nil
                    :height (face-attribute 'default :height))
(set-face-attribute 'doom-neotree-file-face nil
                    :height (face-attribute 'default :height))
(set-face-attribute 'doom-neotree-file-face nil
                    :foreground (face-attribute 'default :foreground))

;; Enable file icons in neotree
(setq doom-neotree-enable-file-icons t)

;; To easily show/hide the neotree drawer
(global-set-key [f8] 'neotree-toggle)

;; Don't show the neotree drawer when in ediff mode
(add-hook 'ediff-before-setup-windows-hook 'neotree-hide)

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
  (add-hook 'after-init-hook 'dimmer-mode))


(provide 'init-themes)
