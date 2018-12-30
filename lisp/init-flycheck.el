(when (maybe-require-package 'flycheck)
  (add-hook 'after-init-hook 'global-flycheck-mode)
  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)

  (when (maybe-require-package 'flycheck-color-mode-line)
    (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)))

;; customize flycheck temp file prefix
;; http://codewinds.com/blog/2015-04-02-emacs-flycheck-eslint-jsx.html
(setq-default flycheck-temp-prefix ".flycheck")

(provide 'init-flycheck)
