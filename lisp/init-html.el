;; init-html.el --- Editing HTML -*- lexical-binding: t -*-
;;; Commentary:

;; ERB is configured separately in init-ruby

;;; Code:

(require 'sgml-mode)

(when (maybe-require-package 'web-mode)
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode)))


(with-eval-after-load 'web-mode
  (setq web-mode-markup-indent-offset 2))

(defun my/enable-minor-mode (my-pair)
  "Enable minor mode if filename match the regexp.  MY-PAIR is a cons cell (regexp . minor-mode)."
  (if (buffer-file-name)
      (if (string-match (car my-pair) buffer-file-name)
          (funcall (cdr my-pair)))))

(when (maybe-require-package 'prettier-js)
  (add-hook 'web-mode-hook #'(lambda ()
                               (my/enable-minor-mode
                                '("\\.html?\\'" . prettier-js-mode)))))

(defun my/turn-on-sgml-electric-tag-pair-mode ()
  (sgml-electric-tag-pair-mode 1))
(add-hook 'web-mode-hook 'my/turn-on-sgml-electric-tag-pair-mode)

(provide 'init-html)
;;; init-html.el ends here
