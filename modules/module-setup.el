;;; module-setup.el --- setup.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Управление пакетами: setup.el и дополнительные макросы.
;; Данный набор макросов преследует ту же цель, что и use-package,
;; но достигает этого меньшим объемом кода, что положительно сказывается
;; на скорости инициализации Emacs.

;; https://github.com/raxod502/straight.el
;; https://www.emacswiki.org/emacs/SetupEl
;; https://git.sr.ht/~pkal/setup

;;; Code:

(straight-use-package '(setup :type git :host nil :repo "https://git.sr.ht/~pkal/setup"))
(require 'setup)

(setup-define :straight
    (lambda (recipe)
        `(unless (straight-use-package ',recipe)
             ,(setup-quit)))
    :documentation "Install RECIPE with `straight-use-package'.
This macro can be used as HEAD, and will replace itself with the
first RECIPE's package."
    :repeatable t
    :shorthand (lambda (sexp)
                   (let ((recipe (cadr sexp)))
                       (if (consp recipe)
                               (car recipe)
                           recipe))))

(setup-define :eval-after
    (lambda (features &rest body)
        (let ((body `(progn ,@body))
              (features (if (listp features)
                                features
                            (list features))))
            (dolist (feature (nreverse features))
                (setq body `(with-eval-after-load ',feature ,body)))
            body))
    :documentation "Evaluate BODY after FEATURES are loaded."
    :indent 1)

(setup-define :advice
    (lambda (symbol where function)
        `(advice-add ',symbol ,where ,(setup-ensure-function function)))
    :documentation "Add a piece of advice on a function.
See `advice-add' for more details."
    :after-loaded t
    :debug '(sexp sexp function-form)
    :repeatable t)

(setup-define :with-local-quit
    (lambda (&rest body)
        `(catch ',(setup-get 'quit)
             ,@body))
    :documentation "Prevent any reason to abort from leaving beyond BODY."
    :debug '(setup))

(setup-define :quit
    #'setup-quit
    :documentation "Unconditionally abort the evaluation of the current body.")

(provide 'module-setup)
;;; module-setup.el ends here
