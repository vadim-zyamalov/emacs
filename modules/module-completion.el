;;; module-completion.el --- Completion -*- lexical-binding: t; -*-

;;; Commentary:

;; Пакеты, добавляющие и дополняющие автодополнение.

;; https://github.com/emacs-lsp/lsp-mode
;; https://github.com/minad/cape
;; https://github.com/minad/corfu
;; https://github.com/jdtsmith/kind-icon
;; https://github.com/minad/cape
;; https://github.com/minad/vertico
;; https://github.com/oantolin/orderless
;; https://github.com/minad/consult
;; https://github.com/oantolin/embark
;; https://github.com/minad/tempel
;; https://github.com/joaotavora/yasnippet
;; https://github.com/AndreaCrotti/yasnippet-snippets
;; https://github.com/mohkale/consult-yasnippet

;;; Code:

;; LSP
(defun lsp/non-greedy-lsp-mode ()
    "Making LSP capf non-greedy."
    (progn
        (fset 'non-greedy-lsp
              (cape-capf-buster
               (cape-capf-properties #'lsp-completion-at-point :exclusive 'no)))
        (setq completion-at-point-functions
              (list #'non-greedy-lsp))))

(defun lsp/extra-capf ()
    "Adding extra capf during LSP startup."
    (let ((tmp-symbol (intern (concat "capf/" (symbol-name major-mode)))))
        (unless (null (symbol-function tmp-symbol))
            (funcall (symbol-function tmp-symbol)))))

(use-package lsp-mode
    :straight t
    :commands (lsp lsp-deferred)
    :hook ((lsp-mode . lsp-enable-which-key-integration)
           (lsp-completion-mode . (lambda ()
                                      (progn
                                          (lsp/non-greedy-lsp-mode)
                                          (lsp/extra-capf)))))
    :config
    (setq lsp-headerline-breadcrumb-icons-enable nil
          lsp-enable-file-watchers nil
          lsp-keymap-prefix "C-c l")
    (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
    :custom
    (lsp-completion-provider :none))


;; Corfu
(use-package corfu
    :straight t
    :init
    (setq tab-always-indent 'complete)
    (corfu-global-mode)
    :bind (:map corfu-map
                ("TAB" . corfu-next)
                ([tab] . corfu-next)
                ("S-TAB" . corfu-previous)
                ([backtab] . corfu-previous))
    :custom
    (corfu-cycle t)
    (corfu-auto nil)
    (corfu-preselect-first nil)
    (corfu-preview-current 'insert))

(use-package kind-icon
    :straight t
    :after corfu
    :custom
    (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
    :config
    (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))


;; Cape
(use-package cape
    :straight t
    :init
    (add-to-list 'completion-at-point-functions #'cape-file t))


;; Vertico
(use-package vertico
    :straight t
    :hook
    ((minibuffer-setup . (lambda ()
                             (setq completion-in-region-function
                                   (if vertico-mode
                                           #'consult-completion-in-region
                                       #'completion--in-region)))))
    :init
    (vertico-mode)
    :custom
    (vertico-cycle t))

(use-package consult
    :straight t
    :bind (("C-s" . consult-line)
           ("<f2>" . consult-buffer)
           ("C-<f2>" . ibuffer)))


;; Orderless
(use-package orderless
    :straight t
    :init
    (setq completion-styles '(orderless)
          completion-category-defaults nil
          completion-category-overrides '((file (styles . (partial-completion))))))


;; Embark
(use-package embark
    :straight t
    :init
    (setq prefix-help-command #'embark-prefix-help-command)
    :bind (("C-." . embark-act)
           ("C-;" . embark-dwim)
           ("C-h B" . embark-bindings)))

(use-package embark-consult
    :straight t
    :demand t
    :after (embark consult)
    :hook (embark-collect-mode . consult-preview-at-point-mode))

;; Сниппеты
(when (string-equal init/snippet-engine "tempel")
    (use-package tempel
    :straight t
    :custom
    (tempel-path (expand-file-name "templates" (file-name-directory user-init-file)))
    :bind (("<f6>" . tempel-complete)
           ("<f7>" . tempel-insert)
           ("C-<f6>" . tempel-done))))

(when (string-equal init/snippet-engine "yasnippet")
    (use-package yasnippet
        :straight t
        :bind (:map yas-minor-mode-map
                    ([(tab)] . nil)
                    ("TAB" . nil))
        :config
        (yas-global-mode 1))

    (use-package yasnippet-snippets
        :straight t)

    (use-package consult-yasnippet
        :straight t
        :bind (("<f7>" . consult-yasnippet))))


(provide 'module-completion)
;;; module-completion.el ends here
