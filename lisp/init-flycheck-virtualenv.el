(require 'flycheck)

;; From: https://github.com/lunaryorn/old-emacs-configuration/blob/master/lisp/flycheck-virtualenv.el
;; We can safely declare this function, since we'll only call it in Python Mode,
;; that is, when python.el was already loaded.
(declare-function python-shell-calculate-exec-path "python")

(defun flycheck-virtualenv-executable-find (executable)
  "Find an EXECUTABLE in the current virtualenv if any."
  (if (bound-and-true-p python-shell-virtualenv-root)
      (let ((exec-path (python-shell-calculate-exec-path)))
        (executable-find executable))
    (flycheck-default-executable-find executable)))

(defun flycheck-virtualenv-setup ()
  "Setup Flycheck for the current virtualenv."
  (setq-local flycheck-executable-find #'flycheck-virtualenv-executable-find))

(when (maybe-require-package 'flycheck)
  (add-hook 'python-mode-hook #'flycheck-virtualenv-setup))

(provide 'init-flycheck-virtualenv)
