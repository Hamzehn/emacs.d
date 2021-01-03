;;; init-flycheck.el --- Configure Flycheck global behaviour -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'flycheck)
  (add-hook 'after-init-hook 'global-flycheck-mode)
  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)
  (with-eval-after-load 'flycheck
    ;; disable jshint since we prefer eslint checking
    ;; http://codewinds.com/blog/2015-04-02-emacs-flycheck-eslint-jsx.html
    (setq flycheck-disabled-checkers
          (append flycheck-disabled-checkers
                  '(javascript-jshint)))
    ;; customize flycheck temp file prefix
    ;; http://codewinds.com/blog/2015-04-02-emacs-flycheck-eslint-jsx.html
    (setq-default flycheck-temp-prefix ".flycheck"))
  (when (maybe-require-package 'flycheck-color-mode-line)
    (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)))

(provide 'init-flycheck)
;;; init-flycheck.el ends here
