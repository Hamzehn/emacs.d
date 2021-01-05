;;; init-python.el --- Python editing -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq auto-mode-alist
      (append '(("SConstruct\\'" . python-mode)
                ("SConscript\\'" . python-mode))
              auto-mode-alist))

(setq python-shell-interpreter "python3")

;; Use Elpy for Python (alternative would be anaconda-mode)
(when (maybe-require-package 'elpy)
  (elpy-enable)
  (setq elpy-rpc-virtualenv-path 'default)
  (when (maybe-require-package 'flycheck)
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    (add-hook 'elpy-mode-hook 'flycheck-mode))
  (setq elpy-rpc-backend "jedi"))

;; Emacs IPython Notebook EIN config
(when (maybe-require-package 'ein)
  ;; use autocompletion, but don't start to autocomplete after a dot
  (setq ein:complete-on-dot -1)
  (setq ein:use-auto-complete t)
  (setq ein:query-timeout 1000))

(when (maybe-require-package 'toml-mode)
  (add-to-list 'auto-mode-alist '("poetry\\.lock\\'" . toml-mode)))

(when (maybe-require-package 'reformatter)
  (reformatter-define black :program "black"))

(provide 'init-python)
;;; init-python.el ends here
