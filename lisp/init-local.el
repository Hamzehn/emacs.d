;; Themes

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
