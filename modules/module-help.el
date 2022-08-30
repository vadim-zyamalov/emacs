;;; module-help.el --- help -*- lexical-binding: t; -*-

;;; Commentary:

;; Пакеты, облегчающие получение справки.
;; Если конкретнее, то данные пакеты добавляют дополнительную информацию,
;; например, Marginalia добавляет новую информацию в M-x, DirEd.

;; https://github.com/minad/marginalia
;; https://github.com/justbur/emacs-which-key
;; https://github.com/Wilfred/helpful

;;; Code:

(setup (:straight marginalia)
    (marginalia-mode))

(setup (:straight which-key)
    (:option which-key-idle-delay 1)
    (which-key-mode))

(setup (:straight helpful)
    (:global [remap describe-function] helpful-callable
             [remap describe-variable] helpful-variable
             [remap describe-key] helpful-key
             "C-h F" helpful-function
             "C-h C" helpful-command))

(provide 'module-help)
;;; module-help.el ends here
