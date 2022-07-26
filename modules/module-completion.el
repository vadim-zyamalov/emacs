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
    (lsp-completion-provider :none))


;; Corfu
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
    (corfu-doc-terminal-mode t))

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
(use-package tempel
    :straight t
    :if (string-equal init/snippet-engine "tempel")
    :bind (("<f6>" . tempel-complete)
           ("<f7>" . tempel-insert)
           ("C-<f6>" . tempel-done))
    :custom
    (tempel-path (expand-file-name
                  "templates"
                  (file-name-directory user-init-file))))

(use-package yasnippet
    :straight t
    :if (string-equal init/snippet-engine "yasnippet")
    :bind (:map yas-minor-mode-map
                ([(tab)] . nil)
                ("TAB" . nil))
    :config
    (yas-global-mode 1))

(use-package yasnippet-snippets
    :straight t
    :if (string-equal init/snippet-engine "yasnippet"))

(use-package consult-yasnippet
    :straight t
    :if (string-equal init/snippet-engine "yasnippet")
    :bind ("<f7>" . consult-yasnippet))


(provide 'module-completion)
;;; module-completion.el ends here
