;;; init-corfu.el --- Interactive completion in buffers -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; WAITING: haskell-mode sets tags-table-list globally, breaks tags-completion-at-point-function
;; TODO Default sort order should place [a-z] before punctuation

(setq tab-always-indent 'complete)
(when (maybe-require-package 'orderless)
  (with-eval-after-load 'vertico
    (require 'orderless)
    (setq completion-styles '(orderless basic))))
(setq completion-category-defaults nil
      completion-category-overrides nil)
(setq completion-cycle-threshold 4)

(when (maybe-require-package 'corfu)
  (setq-default corfu-auto t)
  (setq-default corfu-auto-prefix 2)
  (setq-default corfu-auto-delay 0.0)
  (setq-default corfu-quit-at-boundary t)
  (setq-default corfu-cycle nil)
  (setq-default corfu-quit-no-match 'separator)

  (with-eval-after-load 'corfu
    (face-spec-set 'corfu-current
                   '((t (:inherit 'vertico-current :background nil))))
    (corfu-popupinfo-mode)
    (set-face-attribute 'corfu-popupinfo nil :height 0.8)
    (corfu-history-mode 1))

  (when (maybe-require-package 'kind-icon)
    (with-eval-after-load 'corfu
      (setq kind-icon-default-style
            '( :padding -1.1
               :stroke 0
               :margin 0.05
               :radius 0
               :height 0.45
               :scale 1 ))
      (setq kind-icon-default-face 'corfu-default)
      (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)))

  ;; Disable auto completion in eshell and shell modes
  (with-eval-after-load 'eshell
    (add-hook 'eshell-mode-hook (lambda () (setq-local corfu-auto nil))))
  (with-eval-after-load 'shell
    (add-hook 'shell-mode-hook (lambda () (setq-local corfu-auto nil))))

  (add-hook 'after-init-hook 'global-corfu-mode))

(provide 'init-corfu)
;;; init-corfu.el ends here
