;; Themes

(setenv "WORKON_HOME" "/opt/anaconda/envs")

(setq eldoc-idle-delay 1)  ;; in second
(setq auto-window-vscroll nil)

;; use git bash shell on windows
(if (eq system-type 'windows-nt)
    (progn
      (setq explicit-shell-file-name "c:/Program Files/Git/bin/bash.exe")
      (setq shell-file-name explicit-shell-file-name)
      (setq explicit-sh.exe-args '("--login" "-i"))
      (add-to-list 'exec-path "c:/Program Files/Git/bin/bash.exe")
      (setenv "SHELL" shell-file-name)
      (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)
      )
  )

;; show the menu bar
(menu-bar-mode 1)

;; Setup frame size and windows
(add-to-list 'default-frame-alist '(top . 5))
(add-to-list 'default-frame-alist '(right . 200))
(add-to-list 'default-frame-alist '(height . 60))
(add-to-list 'default-frame-alist '(width . 200))
;; Set the font
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "outline" :slant normal :weight normal :height 105 :width normal)))))

(provide 'init-local)
