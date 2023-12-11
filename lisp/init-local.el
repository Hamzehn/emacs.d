;; Themes

;; Set idle delay for eldoc-mode
(with-eval-after-load 'eldoc-mode
  (defun my/eldoc-mode-hook ()
    (setq eldoc-idle-delay 0.5))
  (add-hook 'eldoc-mode-hook 'my/eldoc-mode-hook))

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

;; Set the default font
(when (member "DejaVu Sans Mono" (font-family-list))
  (set-face-attribute 'default nil :font "DejaVu Sans Mono" :slant 'normal
                      :weight 'normal :height 110 :width 'normal))
;; specify font for all unicode characters
(when (member "Symbola" (font-family-list))
  (set-fontset-font t 'unicode "Symbola" nil 'prepend))

;; Increase the left fringe by 1px to properly show diff-hl mode markers in right window
(add-to-list 'default-frame-alist '(left-fringe . 9))

;; Set default org mode notes file
(with-eval-after-load 'org
  (setq org-default-notes-file "~/org/inbox.org"))

;; Select the displayed help window by default
(setq help-window-select t)

;; When BROWSER is defined, set the default browser
(when (getenv "BROWSER")
  (setq browse-url-generic-program (executable-find (getenv "BROWSER"))
        browse-url-browser-function 'browse-url-generic))

;; Suppress native compilation warnings
(setq native-comp-async-report-warnings-errors 'silent)

(provide 'init-local)
