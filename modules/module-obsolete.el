;;; module-obsolete.el --- Unused configs -*- lexical-binding: t; -*-

;;; Commentary:

;; В принципе не нужно, но выбросить жалко.

;; https://github.com/joaotavora/eglot
;; https://github.com/company-mode/company-mode

;;; Code:

(setup (:straight eglot)
    (:bind "C-c l r" eglot-rename
           "C-c l o" eglot-code-action-organize-imports
           "C-c l h" eldoc
           "C-c l d" xref-find-definitions)
    (:with-hook eglot-managed-mode-hook
        (:hook (lambda ()
                   (progn
                       (lsp/non-greedy-eglot)
                       (lsp/extra-capf)))))
    (:with-hook python-mode-hook
        (:hook eglot-ensure)))

(defun lsp/non-greedy-eglot ()
    "Making Eglot capf non-greedy."
    (progn
        (fset 'non-greedy-eglot
              (cape-capf-buster
               (cape-capf-properties #'eglot-completion-at-point :exclusive 'no)))
        (setq completion-at-point-functions
              (list #'non-greedy-eglot))))

(defun my/ret-handle ()
    (interactive)
    (if (company-explicit-action-p)
            (company-complete)
        (call-interactively
         (or (key-binding (this-command-keys))
             (key-binding (kbd "RET"))))))

(defun company/inject (ll el &optional pre)
    "Add extra backend(s) to the grouped company backend.

The company backends list should be of the following form:

'((BACKENDS))"
    (let ((ell (if (listp el)
                       el
                   (list el))))
        (if pre
                `(,(append ell (car ll)))
            `(,(append (car ll) ell)))))

(defun lsp/extra-company ()
    (if (string-equal major-mode "python-mode")
            (setq company-backends
                  (company/inject company-backends
                                  'company-files
                                  t)))
    (if (string-equal major-mode "latex-mode")
            (setq company-backends
                  (company/inject company-backends
                                  '(company-reftex-labels
                                    company-reftex-citations
                                    company-math-symbols-latex
                                    company-math-symbols-unicode)))))

(setup (:straight company)
    (:option company-selection-wrap-around t
             company-minimum-prefix-length 1
             company-idle-delay nil
             company-tooltip-align-annotations t
             company-transformers '(delete-consecutive-dups
                                    company-sort-by-occurrence
                                    company-sort-prefer-same-case-prefix))
    (:bind [remap indent-for-tab-command] company-indent-or-complete-common)
    (:bind-into company-active-map
        "RET" my/ret-handle
        "<return>" my/ret-handle
        "<tab>" company-complete-common-or-cycle)
    (:with-hook after-init-hook
        (:hook global-company-mode))
    (setq company-backends '((company-capf))))

(setup (:straight company-box)
    (:hook-into company-mode))

(provide 'module-obsolete)
;;; module-obsolete.el ends here
