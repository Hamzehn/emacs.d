;;; init-python.el --- Python editing -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq auto-mode-alist
      (append '(("SConstruct\\'" . python-mode)
                ("SConscript\\'" . python-mode))
              auto-mode-alist))

(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "--simple-prompt -i")

(require-package 'pip-requirements)

;; Use Elpy for Python (alternative would be anaconda-mode)
(when (maybe-require-package 'elpy)
  (elpy-enable)
  (setq elpy-rpc-virtualenv-path 'default)
  (setq elpy-rpc-backend "jedi"))

(when (maybe-require-package 'pyvenv)
  (pyvenv-tracking-mode))

;; Emacs IPython Notebook EIN config
(when (maybe-require-package 'ein)
  (setq ein:use-auto-complete t)
  (setq ein:output-area-inlined-images t)
  (setq ein:query-timeout 1500)
  (add-hook 'ein:notebook-mode-hook 'turn-off-fci-mode)
  )

(when (maybe-require-package 'toml-mode)
  (add-to-list 'auto-mode-alist '("poetry\\.lock\\'" . toml-mode)))

(when (maybe-require-package 'reformatter)
  (reformatter-define black :program "black" :args '("-")))

(when (maybe-require-package 'sphinx-doc)
  (add-hook 'python-mode-hook
            (lambda () (sphinx-doc-mode t))))

(provide 'init-python)
;;; init-python.el ends here
