;; Themes

;;(setenv "WORKON_HOME" "/opt/anaconda/envs")

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

;; From: https://www.emacswiki.org/emacs/DiredSortDirectoriesFirst
;; Sort dired listings with directories first
(defun mydired-sort ()
  "Sort dired listings with directories first."
  (save-excursion
    (let (buffer-read-only)
      (forward-line 2) ;; beyond dir. header
      (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max)))
    (set-buffer-modified-p nil)))

(defadvice dired-readin
    (after dired-after-updating-hook first () activate)
  "Sort dired listings with directories first before adding marks."
  (mydired-sort))

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

;; Increase the left fringe by 1px to properly show diff-hl mode markers in right window
(add-to-list 'default-frame-alist '(left-fringe . 9))

(provide 'init-local)
