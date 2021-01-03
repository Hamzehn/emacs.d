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
(add-to-list 'default-frame-alist '(top . 40))
(add-to-list 'default-frame-alist '(left . (+ 20)))
(add-to-list 'default-frame-alist '(height . 56))
(add-to-list 'default-frame-alist '(width . 200))

;; Set the default font
(when (member "DejaVu Sans Mono" (font-family-list))
  (set-face-attribute 'default nil :font "DejaVu Sans Mono" :slant 'normal
                      :weight 'normal :height 105 :width 'normal))
;; specify font for all unicode characters
(when (member "Symbola" (font-family-list))
  (set-fontset-font t 'unicode "Symbola" nil 'prepend))

;; Increase the left fringe by 1px to properly show diff-hl mode markers in right window
(add-to-list 'default-frame-alist '(left-fringe . 9))

(provide 'init-local)
