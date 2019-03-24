;;; init-python.el --- Python editing -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Elpy for Python
(require-package 'elpy)
(elpy-enable)
(remove-hook 'elpy-modules 'elpy-module-flymake)

;; ---------------------------------
;; Emacs IPython Notebook EIN config
;; ---------------------------------
(require-package 'ein)
;; use autocompletion, but don't start to autocomplete after a dot
(setq ein:complete-on-dot -1)
(setq ein:use-auto-complete t)

;; timeout settings
(setq ein:query-timeout 1000)

(setq auto-mode-alist
      (append '(("SConstruct\\'" . python-mode)
                ("SConscript\\'" . python-mode))
              auto-mode-alist))

(require-package 'pip-requirements)

(when (maybe-require-package 'anaconda-mode)
  (after-load 'python
    (add-hook 'python-mode-hook 'anaconda-mode)
    (add-hook 'python-mode-hook 'anaconda-eldoc-mode))
  (when (maybe-require-package 'company-anaconda)
    (after-load 'company
      (after-load 'python
        (push 'company-anaconda company-backends)))))

(provide 'init-python)
;;; init-python.el ends here
