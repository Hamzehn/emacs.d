;;; init-python.el --- Python editing -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Elpy for Python
(require-package 'elpy)
(elpy-enable)
;; (when (require 'flycheck nil t)
(when (maybe-require-package 'flycheck)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))
;;(remove-hook 'elpy-modules 'elpy-module-flymake)

;; Autopep8
;; (require-package 'py-autopep8)
;; (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

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


(provide 'init-python)
;;; init-python.el ends here
