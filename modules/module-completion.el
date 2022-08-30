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
(setup (:straight lsp-mode)
    (:option lsp-headerline-breadcrumb-icons-enable nil
             lsp-enable-file-watchers nil
             lsp-keymap-prefix "C-c l"
             lsp-completion-provider :none)
    (with-eval-after-load 'lsp-mode
        (define-key lsp-mode-map (kbd "C-c l") lsp-command-map))
    (:hook lsp-enable-which-key-integration)
    (:with-mode lsp-completion-mode
        (:hook (lambda ()
                   (progn
                       (lsp/non-greedy-lsp-mode)
                       (lsp/extra-capf)))))))

(when (string-equal init/lsp-engine "eglot")
    (setup (:straight eglot)
        (:bind "C-c l r" eglot-rename
               "C-c l o" eglot-code-action-organize-imports
               "C-c l h" eldoc
               "C-c l d" xref-find-definitions)
        (:with-mode eglot-managed-mode
            (:hook (lambda ()
                       (progn
                           (lsp/non-greedy-eglot)
                           (lsp/extra-capf)))))))


;; Corfu
(when (string-equal init/completion-popup "corfu")
    (setup (:straight corfu
                      ;;corfu-doc
                      kind-icon
                      (cape
                       :type git
                       :host github
                       :repo "minad/cape"))
        ;;(popon
        ;; :type git
        ;; :repo "https://codeberg.org/akib/emacs-popon.git")
        ;;(corfu-terminal
        ;; :type git
        ;; :repo "https://codeberg.org/akib/emacs-corfu-terminal.git")
        ;;(corfu-doc-terminal
        ;; :type git
        ;; :repo "https://codeberg.org/akib/emacs-corfu-doc-terminal.git"))
        (:option corfu-auto nil
                 corfu-cycle t
                 corfu-preselect-first nil
                 corfu-preview-current 'insert
                 tab-always-indent 'complete
                 kind-icon-default-face 'corfu-default)
        ;;(:hook corfu-doc-mode)
        (:bind-into corfu-map
            "TAB" corfu-next
            [tab] corfu-next
            "S-TAB" corfu-previous
            [backtab] corfu-previous)
        (global-corfu-mode)
        (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
        (add-to-list 'completion-at-point-functions #'cape-file t))
;;(unless (display-graphic-p)
;;    (corfu-terminal-mode t)
;;    (corfu-doc-terminal-mode t)))
    )

(when (string-equal init/completion-popup "company")
    (defun my/ret-handle ()
        (interactive)
        (if (company-explicit-action-p)
                (company-complete)
            (call-interactively
             (or (key-binding (this-command-keys))
                 (key-binding (kbd "RET"))))))

    (setup (:straight company
                      company-box)
        (:option company-backends '((company-capf))
                 company-selection-wrap-around t
                 company-minimum-prefix-length 1
                 company-idle-delay nil
                 company-tooltip-align-annotations t
                 company-transformers '(delete-consecutive-dups
                                        company-sort-by-occurrence
                                        company-sort-prefer-same-case-prefix))
        (:global [remap indent-for-tab-command] company-indent-or-complete-common)
        (:with-map company-active-map
            (:bind "RET" my/ret-handle
                   "<return>" my/ret-handle
                   "<tab>" company-complete-common-or-cycle))
        (:hook company-box-mode)
        (:with-hook after-init
            (:hook global-company-mode))))

;; Vertico+Embark
(setup (:straight vertico
                  consult
                  embark
                  embark-consult
                  orderless)
    (:option vertico-cycle t
             vertico-mouse-mode t
             vertico-count 8
             vertico-resize t
             prefix-help-command #'embark-prefix-help-command
             completion-styles '(orderless basic)
             completion-category-defaults nil
             completion-category-overrides '((file (styles basic partial-completion))))
    (:global "<f2>" consult-buffer
             "C-<f2>" ibuffer
             "C-." embark-act
             "C-;" embark-dwim
             "C-h B" embark-bindings)
    (:with-hook minibuffer-setup-hook
        (:hook (lambda ()
                   (setq completion-in-region-function
                         (if vertico-mode
                                 #'consult-completion-in-region
                             #'completion--in-region)))))
    (vertico-mode)
    (:with-mode embark-collect-mode
        (:hook consult-preview-at-point-mode)))


;; Сниппеты
(when (string-equal init/snippet-engine "tempel")
    (setup (:straight tempel)
        (:option tempel-path (expand-file-name
                              "templates"
                              (file-name-directory user-init-file)))
        (:global "<f6>" tempel-complete
                 "<f7>" tempel-insert
                 "C-<f6>" tempel-done)))

(when (string-equal init/snippet-engine "yasnippet")
    (setup (:straight yasnippet)
        (:bind-into yas-minor-mode-map
            [(tab)] nil
            "TAB" nil)
        (yas-global-mode 1))

    (setup (:straight yasnippet-snippets))

    (setup (:straight consult-yasnippet)
        (:global "<f7>" consult-yasnippet)))


(provide 'module-completion)
;;; module-completion.el ends here
