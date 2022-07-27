;;; module-completion.el --- Completion -*- lexical-binding: t; -*-

;;; Commentary:

;; Пакеты, добавляющие и дополняющие автодополнение.

;; https://github.com/emacs-lsp/lsp-mode
;; https://github.com/joaotavora/eglot
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
(defun lsp/lsp ()
    "Using an appropriate LSP-engine."
    (cond ((string-equal init/lsp-engine "lsp")
           (lsp))
          ((string-equal init/lsp-engine "eglot")
           (eglot-ensure))
          (t
           (error "Unknown LSP-engine `%s'" init/lsp-engine))))

(defun lsp/non-greedy-lsp-mode ()
    "Making LSP capf non-greedy."
    (progn
        (fset 'non-greedy-lsp
              (cape-capf-buster
               (cape-capf-properties #'lsp-completion-at-point :exclusive 'no)))
        (setq completion-at-point-functions
              (list #'non-greedy-lsp))))

(defun lsp/non-greedy-eglot ()
    "Making Eglot capf non-greedy."
    (progn
        (fset 'non-greedy-eglot
              (cape-capf-buster
               (cape-capf-properties #'eglot-completion-at-point :exclusive 'no)))
        (setq completion-at-point-functions
              (list #'non-greedy-eglot))))

(defun lsp/extra-capf ()
    "Adding extra capf during LSP startup."
    (let ((tmp-symbol (intern (concat "capf/" (symbol-name major-mode)))))
        (unless (null (symbol-function tmp-symbol))
            (funcall (symbol-function tmp-symbol)))))

(when (string-equal init/lsp-engine "lsp")
    (use-package lsp-mode
        :straight t
        :hook ((lsp-mode . lsp-enable-which-key-integration)
               (lsp-completion-mode . (lambda ()
                                          (progn
                                              (lsp/non-greedy-lsp-mode)
                                              (lsp/extra-capf)))))
        :config
        (with-eval-after-load 'lsp-mode
            (define-key lsp-mode-map (kbd "C-c l") lsp-command-map))
        :custom
        (lsp-headerline-breadcrumb-icons-enable nil)
        (lsp-enable-file-watchers nil)
        (lsp-keymap-prefix "C-c l")
        (lsp-completion-provider :none)))

(when (string-equal init/lsp-engine "eglot")
    (use-package eglot
        :straight t
        :hook (eglot-managed-mode . (lambda ()
                                        (progn
                                            (lsp/non-greedy-eglot)
                                            (lsp/extra-capf))))
        :bind (:map eglot-map
                    ("C-c l r" . eglot-rename)
                    ("C-c l o" . eglot-code-action-organize-imports)
                    ("C-c l h" . eldoc)
                    ("C-c l d" . xref-find-definitions))))


;; Corfu or Company
(when (string-equal init/completion-popup "corfu")
    (use-package corfu
        :straight t
        :bind (:map corfu-map
                    ("TAB" . corfu-next)
                    ([tab] . corfu-next)
                    ("S-TAB" . corfu-previous)
                    ([backtab] . corfu-previous))
        :init
        (global-corfu-mode)
        :custom
        (corfu-auto nil)
        (corfu-cycle t)
        (corfu-preselect-first nil)
        (corfu-preview-current 'insert)
        (tab-always-indent 'complete))

    (use-package corfu-doc
        :straight t
        :hook (corfu-mode . corfu-doc-mode))

    (use-package kind-icon
        :straight t
        :after corfu
        :config
        (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
        :custom
        (kind-icon-default-face 'corfu-default))

    (use-package popon
        :straight (popon
                   :type git
                   :repo "https://codeberg.org/akib/emacs-popon.git"))

    (use-package corfu-terminal
        :straight (corfu-terminal
                   :type git
                   :repo "https://codeberg.org/akib/emacs-corfu-terminal.git")
        :unless (display-graphic-p)
        :after corfu
        :config
        (corfu-terminal-mode t))

    (use-package corfu-doc-terminal
        :straight (corfu-doc-terminal
                   :type git
                   :repo "https://codeberg.org/akib/emacs-corfu-doc-terminal.git")
        :unless (display-graphic-p)
        :after corfu-doc
        :config
        (corfu-doc-terminal-mode t)))

(when (string-equal init/completion-popup "company")
    (defun my/ret-handle ()
        (interactive)
        (if (company-explicit-action-p)
                (company-complete)
            (call-interactively
             (or (key-binding (this-command-keys))
                 (key-binding (kbd "RET"))))))

    (use-package company
        :straight t
        :bind (([remap indent-for-tab-command] . company-indent-or-complete-common)
               :map company-active-map
               ("RET" . my/ret-handle)
               ("<return>" . my/ret-handle)
               ("<tab>" . company-complete-common-or-cycle))
        :hook (after-init . global-company-mode)
        :init
        (setq company-backends '((company-capf)))
        :custom
        (company-selection-wrap-around t)
        (company-minimum-prefix-length 1)
        (company-idle-delay nil)
        (company-tooltip-align-annotations t)
        (company-transformers '(delete-consecutive-dups
                                company-sort-by-occurrence
                                company-sort-prefer-same-case-prefix)))

    (use-package company-box
        :straight t
        :hook (company-mode . company-box-mode)))

(use-package cape
    :straight t
    :config
    (add-to-list 'completion-at-point-functions #'cape-file t))


;; Vertico
(use-package vertico
    :straight t
    :hook (minibuffer-setup . (lambda ()
                                  (setq completion-in-region-function
                                        (if vertico-mode
                                                #'consult-completion-in-region
                                            #'completion--in-region))))
    :init
    (vertico-mode))

(use-package consult
    :straight t
    :bind (("<f2>" . consult-buffer)
           ("C-<f2>" . ibuffer)))


;; Embark
(use-package embark
    :straight t
    :bind (("C-." . embark-act)
           ("C-;" . embark-dwim)
           ("C-h B" . embark-bindings))
    :custom
    (prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
    :straight t
    :after (embark consult)
    :hook (embark-collect-mode . consult-preview-at-point-mode))


;; Orderless
(use-package orderless
    :straight t
    :custom
    (completion-styles '(orderless basic))
    (completion-category-defaults nil)
    (completion-category-overrides '((file (styles basic partial-completion)))))


;; Сниппеты
(when (string-equal init/snippet-engine "tempel")
    (use-package tempel
        :straight t
        :bind (("<f6>" . tempel-complete)
               ("<f7>" . tempel-insert)
               ("C-<f6>" . tempel-done))
        :custom
        (tempel-path (expand-file-name
                      "templates"
                      (file-name-directory user-init-file)))))

(when (string-equal init/snippet-engine "yasnippet")
    (use-package yasnippet
        :straight t
        ;; :bind (:map yas-minor-mode-map
        ;;             ([(tab)] . nil)
        ;;             ("TAB" . nil))
        :config
        (yas-global-mode 1))

    (use-package yasnippet-snippets
        :straight t)

    (use-package consult-yasnippet
        :straight t
        :bind ("<f7>" . consult-yasnippet)))


(provide 'module-completion)
;;; module-completion.el ends here
