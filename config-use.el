(when (< emacs-major-version 29)
    (straight-use-package 'use-package))

(require 'use-package)

(use-package use-package-hydra
    :straight t)

(defconst ensure/is64
    (not (null
          (string-match "^x86_64-.*" system-configuration)))
    "Equals t if Emacs works on 64-bit system.")

(defconst ensure/isWindows
    (memq system-type '(cygwin windows-nt ms-dos))
    "Equals t if Emacs works on Windows host system.")

(set-language-environment 'utf-8)
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system
 (if (eq system-type 'windows-nt)
         'utf-16-le
     'utf-8))
(prefer-coding-system 'utf-8)

(fset 'yes-or-no-p 'y-or-n-p)
(setq confirm-kill-emacs 'y-or-n-p)

(setq vc-follow-symlinks t)

(use-package saveplace
    :init
    (setq save-place-file (expand-file-name
                           (format "%s/var/%s"
                                   user-emacs-directory
                                   "save-place.el")))
    :config
    (save-place-mode 1))

(use-package savehist
    :init
    (setq savehist-file (expand-file-name
                    (format "%s/data/%s"
                            user-emacs-directory
                            "savehist.el")))
    :config
    (setq history-delete-duplicates t
          history-length 25)
    (savehist-mode))

(use-package gcmh
    :straight t
    :config
    (gcmh-mode t)
    :custom
    (gcmh-verbose t)
    (gcmh-low-cons-threshold (* 8 1024 1024)))

(use-package no-littering
    :straight t
    :after savehist
    :init
    (setq no-littering-etc-directory
          (expand-file-name "config/" user-emacs-directory))
    (setq no-littering-var-directory
          (expand-file-name "data/" user-emacs-directory)))

(setq create-lockfiles nil)

(setq backup-directory-alist `(("." . "~/.saves"))
      backup-by-copying-when-linked t)

(add-hook 'before-save-hook
          (lambda ()
              (when buffer-file-name
                  (let ((dir (file-name-directory buffer-file-name)))
                      (when (and (not (file-exists-p dir))
                                 (y-or-n-p (format "Directory %s does not exist. Create it? " dir)))
                          (make-directory dir t))))))

(unless ensure/isWindows
    (add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p))

(use-package dired
    :init
    (setq dired-recursive-deletes 'top))

(setq frame-resize-pixelwise t)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq split-width-threshold 80)

(setq-default cursor-type 'bar)

(use-package tab-line
    :demand t
    :bind (("M-<left>" . previous-buffer)
           ("M-<right>" . next-buffer))
    :config
    (global-tab-line-mode t))

(use-package doom-modeline
    :straight t
    :hook (after-init . doom-modeline-mode)
    :custom
    (doom-modeline-height 24)
    (doom-modeline-minor-modes t))

(use-package minions
    :straight t
    :config
    (minions-mode t))

(use-package nyan-mode
    :straight t
    :config
    (nyan-mode))

(use-package dashboard
    :straight t
    :after (nerd-icons)
    :config
    (dashboard-setup-startup-hook)
    :custom
    (dashboard-display-icons-p t)
    (dashboard-icon-type 'nerd-icons)
    (dashboard-set-heading-icons t)
    (dashboard-set-file-icons t)
    (dashboard-items '((recents . 15)
                       (projects . 5)))
    (dashboard-startup-banner (expand-file-name
                               "emacs.png"
                               (file-name-directory user-init-file)))
    (dashboard-set-navigator t)
    (dashboard-navigator-buttons
     `((
        (,(nerd-icons-sucicon "nf-custom-emacs" :height 1.0 :v-adjust 0.0)
         "Настройки"
         "Открыть файл с настройками (init.el)"
         (lambda (&rest _)
             (find-file (concat (file-name-directory user-init-file) "config-use.org"))))
        (,(nerd-icons-faicon "nf-fa-github" :height 1.0 :v-adjust 0.0)
         "dotfiles"
         "Github с конфигурационными файлами"
         (lambda (&rest _) (browse-url "https://github.com/vadim-zyamalov/dotfiles")))
        (,(nerd-icons-faicon "nf-fa-github" :height 1.0 :v-adjust 0.0)
         "emacs"
         "Github с настройками Emacs"
         (lambda (&rest _) (browse-url "https://github.com/vadim-zyamalov/emacs")))
        ))))

(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)

(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
(global-visual-line-mode t)

(use-package pulsar
    :straight t
    :config
    (pulsar-global-mode t)
    :custom
    (pulsar-pulse t)
    (pulsar-delay 0.055)
    (pulsar-pulse-functions '(recenter-top-bottom
                              move-to-window-line-top-bottom
                              reposition-window
                              bookmark-jump
                              other-window
                              delete-window
                              delete-other-windows
                              forward-page
                              backward-page
                              scroll-up-command
                              scroll-down-command
                              windmove-right
                              windmove-left
                              windmove-up
                              windmove-down
                              windmove-swap-states-right
                              windmove-swap-states-left
                              windmove-swap-states-up
                              windmove-swap-states-down
                              tab-new
                              tab-close
                              tab-next
                              org-next-visible-heading
                              org-previous-visible-heading
                              org-forward-heading-same-level
                              org-backward-heading-same-level
                              outline-backward-same-level
                              outline-forward-same-level
                              outline-next-visible-heading
                              outline-previous-visible-heading
                              outline-up-heading
                              ctrlf-forward-default
                              ctrlf-backward-default
                              ctrlf-forward-alternate
                              ctrlf-backward-alternate
                              ctrlf-forward-symbol
                              ctrlf-forward-symbol-at-point
                              consult-line)))

(use-package dimmer
    :straight t
    :config
    (dimmer-configure-which-key)
    (add-to-list 'dimmer-buffer-exclusion-regexps "^.*\\*corfu\\*.*$")
    (add-to-list 'dimmer-buffer-exclusion-regexps "^.*\\*corfu-popupinfo\\*.*$")
    (dimmer-mode t)
    :custom
    (dimmer-fraction 0.6)
    (dimmer-watch-frame-focus-events nil))

(use-package framemove
    :straight t
    :after (hydra)
    :bind ("<f6>" . hydra-wind/body)
    :hydra (hydra-wind ()
                       "Moving between windows."
                       ("<left>"  windmove-left  "left")
                       ("<right>" windmove-right "right")
                       ("<up>"    windmove-up    "up")
                       ("<down>"  windmove-down  "down"))
    :custom
    (framemove-hook-into-windmove t))

(use-package ace-window
    :straight t
    :bind (("M-o" . ace-window)))

(use-package treemacs
    :straight t
    :defer t
    :bind (("M-0"       . treemacs-select-window)
           ("C-x t 1"   . treemacs-delete-other-windows)
           ("C-x t t"   . treemacs)
           ("C-x t d"   . treemacs-select-directory)
           ("C-x t B"   . treemacs-bookmark)
           ("C-x t C-t" . treemacs-find-file)
           ("C-x t M-t" . treemacs-find-tag))
    :config
    (treemacs-fringe-indicator-mode 'always)
    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-project-follow-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
        (`(t . t)
         (treemacs-git-mode 'deferred))
        (`(t . _)
         (treemacs-git-mode 'simple))))

(use-package treemacs-magit
    :straight t
    :after (treemacs magit))

(use-package treemacs-nerd-icons
    :straight t
    :after (treemacs nerd-icons)
    :config
    (treemacs-load-theme "nerd-icons"))

(use-package ef-themes
    :straight t
    :init
    (mapc #'disable-theme custom-enabled-themes)
    :config
    (load-theme 'ef-autumn :no-confirm))

(cond ((find-font (font-spec :name "JetBrains Mono"))
       (set-face-attribute 'default
                           nil
                           :font "JetBrains Mono"
                           :height 120))
      ((find-font (font-spec :name "Iosevka"))
       (set-face-attribute 'default
                           nil
                           :font "Iosevka"
                           :height 120))
      ((find-font (font-spec :name "Fira Code"))
       (set-face-attribute 'default
                           nil
                           :font "Fira Code"
                           :height 120)))

(unless (version< emacs-version "28.1")
    (use-package ligature
        :straight (ligature :type git :host github :repo "mickeynp/ligature.el")
        :config
        (ligature-set-ligatures
         'prog-mode
         (pcase (face-attribute 'default :family)
             ("JetBrains Mono"
              '("-|" "-~" "---" "-<<" "-<" "--" "->" "->>" "-->" "///" "/=" "/=="
                "/>" "//" "/*" "*>" "***" "*/" "<-" "<<-" "<=>" "<=" "<|" "<||"
                "<|||" "<|>" "<:" "<>" "<-<" "<<<" "<==" "<<=" "<=<" "<==>" "<-|"
                "<<" "<~>" "<=|" "<~~" "<~" "<$>" "<$" "<+>" "<+" "</>" "</" "<*"
                "<*>" "<->" "<!--" ":>" ":<" ":::" "::" ":?" ":?>" ":=" "::=" "=>>"
                "==>" "=/=" "=!=" "=>" "===" "=:=" "==" "!==" "!!" "!=" ">]" ">:"
                ">>-" ">>=" ">=>" ">>>" ">-" ">=" "&&&" "&&" "|||>" "||>" "|>" "|]"
                "|}" "|=>" "|->" "|=" "||-" "|-" "||=" "||" ".." ".?" ".=" ".-" "..<"
                "..." "+++" "+>" "++" "[||]" "[<" "[|" "{|" "??" "?." "?=" "?:" "##"
                "###" "####" "#[" "#{" "#=" "#!" "#:" "#_(" "#_" "#?" "#(" ";;" "_|_"
                "__" "~~" "~~>" "~>" "~-" "~@" "$>" "^=" "]#"))
             ((or "Fira Code" "Cascadia Code")
              '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                "\\\\" "://"))
             ("Iosevka"
              '("<---" "<--"  "<<-" "<-" "->" "-->" "--->"
                "<->" "<-->" "<--->" "<---->" "<!--" "<==" "<==="
                "<=" "=>" "=>>" "==>" "===>" ">=" "<=>"
                "<==>" "<===>" "<====>" "<!---" "<~~" "<~" "~>"
                "~~>" "::" ":::" "==" "!=" "===" "!=="
                ":=" ":-" ":+" "<*" "<*>" "*>" "<|"
                "<|>" "|>" "+:" "-:" "=:" "<******>" "++"
                "+++"))))
        (global-ligature-mode t)))

(use-package nerd-icons
    :straight t)

(use-package nerd-icons-completion
    :straight t
    :defer 1
    :after (all-the-icons marginalia)
    :config
    (nerd-icons-completion-mode t))

(use-package nerd-icons-dired
    :straight t
    :hook
    (dired-mode . nerd-icons-dired-mode))

(use-package marginalia
    :straight t
    :init
    (marginalia-mode))

(use-package which-key
    :straight t
    :config
    (which-key-mode)
    :custom
    (which-key-idle-delay 1))

(use-package helpful
    :straight t
    :bind (([remap describe-function] . helpful-callable)
           ("<f1> f" . helpful-callable)
           ([remap describe-variable] . helpful-variable)
           ("<f1> v" . helpful-variable)
           ([remap describe-key] . helpful-key)
           ("C-h F" . helpful-function)
           ("C-h C" . helpful-command)))

(setq mouse-wheel-scroll-amount '(1
                                  ((shift) . 5)
                                  ((meta))
                                  ((control) . text-scale))
      mouse-wheel-progressive-speed nil)

(setq auto-window-vscroll nil
      fast-but-imprecise-scrolling t
      scroll-conservatively 101
      scroll-margin 0
      scroll-preserve-screen-position t)

(when (>= emacs-major-version 29)
    (pixel-scroll-precision-mode))

(use-package hydra
    :straight t)

(define-key global-map (kbd "<escape>") 'keyboard-escape-quit)

(define-key global-map (kbd "C-=") #'(lambda ()
                                         (interactive)
                                         (text-scale-set 0)))
(define-key global-map (kbd "C-+") #'(lambda ()
                                         (interactive)
                                         (text-scale-increase 1.1)))
(define-key global-map (kbd "C--") #'(lambda ()
                                         (interactive)
                                         (text-scale-decrease 1.1)))

(define-key global-map (kbd "C-_") nil)

(use-package reverse-im
    :straight t
    :config
    (reverse-im-mode t)
    :custom
    (reverse-im-input-methods '("russian-computer")))

(unless init/evil
    (setq cua-keep-region-after-copy t)
    (cua-mode t)
    (transient-mark-mode t))

(when init/evil
    (use-package evil
        :straight t
        :init
        (setq evil-want-integration t
              evil-want-keybinding nil
              evil-want-C-u-scroll t
              evil-want-C-i-jump nil
              evil-undo-system 'undo-redo
              evil-respect-visual-line-mode t)
        :config
        (evil-mode 1)

        (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
        (evil-global-set-key 'motion "j" 'evil-next-visual-line)
        (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
        (evil-set-initial-state 'messages-buffer-mode 'normal)
        (evil-set-initial-state 'dashboard-mode 'normal))

    (use-package evil-collection
        :straight t
        :after evil
        :config
        (evil-collection-init)))

(delete-selection-mode t)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq require-final-newline t)

(setq-default indent-tabs-mode nil
              tab-width 4
              c-basic-offset 4
              standart-indent 4
              lisp-body-indent 4)

(electric-indent-mode t)

(define-key global-map (kbd "RET") 'newline-and-indent)

(use-package highlight-indent-guides
    :straight t
    :if ensure/isWindows
    :hook (prog-mode . highlight-indent-guides-mode)
    :custom
    (highlight-indent-guides-method 'character)
    (highlight-indent-guides-responsive 'top))

(use-package undo-tree
    :straight t
    :init
    (unbind-key "C-z" global-map)
    (unbind-key "C-_" global-map)
    (unbind-key "C-M-_" global-map)
    :bind (("C-z" . undo-tree-undo)
           ("C-S-z" . undo-tree-redo)
           :map cua--cua-keys-keymap
           ("C-z" . undo-tree-undo))
    :config
    (global-undo-tree-mode)
    :custom
    (undo-tree-history-directory-alist `(("." . ,(format "%s/undo"
                                                        user-emacs-directory)))))

(show-paren-mode t)

(use-package rainbow-delimiters
    :straight t
    :hook ((prog-mode org-mode) . rainbow-delimiters-mode))

(unless init/evil
    (defun comment-or-uncomment-region-or-line ()
        "Comments or uncomments the region or the current line."
        (interactive)
        (let (beg end)
            (if (region-active-p)
                    (setq beg (region-beginning) end (region-end))
                (setq beg (line-beginning-position) end (line-end-position)))
            (comment-or-uncomment-region beg end)
            (forward-line)))

    (global-set-key (kbd "M-;") 'comment-or-uncomment-region-or-line))

(when init/evil
    (use-package evil-nerd-commenter
        :straight t
        :after evil
        :config
        (evilnc-default-hotkeys)))

(unless init/evil
    (defun my/vr/replace ()
        "Replace in whole buffer."
        (interactive)
        (if (region-active-p)
                (call-interactively #'vr/replace)
            (save-excursion
                (goto-char (point-min))
                (call-interactively #'vr/replace))))

    (defun my/vr/query-replace ()
        "Replace in whole buffer."
        (interactive)
        (if (region-active-p)
                (call-interactively #'vr/query-replace)
            (save-excursion
                (goto-char (point-min))
                (call-interactively #'vr/query-replace))))

    (use-package visual-regexp
        :straight t
        :bind (("M-%" . my/vr/replace)
               ("C-M-%" . my/vr/query-replace)
               ("C-c v m" . vr/mc-mark))))

(use-package crux
    :straight t
    :bind (("C-c I" . crux-find-user-init-file)
           ("C-c d" . crux-duplicate-current-line-or-region)
           ("C-c M-d" . crux-duplicate-and-comment-current-line-or-region)
           ("S-<return>" . crux-smart-open-line)
           ("C-S-<return>" . crux-smart-open-line-above)))

(use-package cape
    :straight t
    :config
    (add-to-list 'completion-at-point-functions #'cape-file t))

(defun lsp/lsp ()
    "Using an appropriate LSP-engine."
    (if init/lsp-engine
            (lsp)
        (eglot-ensure)))

(defun lsp/non-greedy-lsp-mode ()
    "Making LSP capf non-greedy."
    (progn
        (fset 'non-greedy-lsp
              (cape-capf-properties #'lsp-completion-at-point :exclusive 'no))
        (setq completion-at-point-functions
              (list #'non-greedy-lsp))))

(defun lsp/non-greedy-eglot ()
    "Making Eglot capf non-greedy."
    (progn
        (fset 'non-greedy-eglot
              (cape-capf-properties #'eglot-completion-at-point :exclusive 'no))
        (setq completion-at-point-functions
              (list #'non-greedy-eglot))))

(defun lsp/extra-capf ()
    "Adding extra capf during LSP startup."
    (let ((tmp-symbol (intern (concat "capf/" (symbol-name major-mode)))))
        (unless (null (symbol-function tmp-symbol))
            (funcall (symbol-function tmp-symbol)))))

(when init/lsp-engine
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

    (use-package lsp-ui
        :straight t))

(unless init/lsp-engine
    (use-package eglot
        :init
        (when (< emacs-major-version 29)
            (straight-use-package 'eglot))
        :hook (eglot-managed-mode . (lambda ()
                                        (progn
                                            (lsp/non-greedy-eglot)
                                            (lsp/extra-capf))))
        :bind (:map eglot-mode-map
                    ("C-c l r" . eglot-rename)
                    ("C-c l o" . eglot-code-action-organize-imports)
                    ("C-c l h" . eldoc)
                    ("C-c l d" . xref-find-definitions))
        :config
        (add-to-list 'eglot-server-programs
                     '(latex-mode . ("texlab")))))

(use-package corfu
    :straight (:files (:defaults "extensions/*"))
    :bind (:map corfu-map
                ("TAB" . corfu-next)
                ([tab] . corfu-next)
                ("S-TAB" . corfu-previous)
                ([backtab] . corfu-previous))
    :init
    (corfu-popupinfo-mode)
    (global-corfu-mode)
    :custom
    (corfu-auto nil)
    (corfu-cycle t)
    (corfu-preselect-first nil)
    (corfu-preview-current 'insert)
    (tab-always-indent 'complete)
    (corfu-popupinfo-delay 0.2))

(use-package kind-icon
    :straight t
    :after (corfu nerd-icons)
    :config
    (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
    :custom
    (kind-icon-default-face 'corfu-default))

(use-package vertico
    :straight (:files (:defaults "extensions/*"))
    :hook ((minibuffer-setup . (lambda ()
                                   (setq completion-in-region-function
                                         (if vertico-mode
                                                 #'consult-completion-in-region
                                             #'completion--in-region))))
           (minibuffer-setup . vertico-repeat-save))
    :init
    (add-to-list 'process-coding-system-alist
                 '("[rR][gG]" . (utf-8-dos . windows-1251-dos)))
    (vertico-mode)
    :bind (("C-c C-r" . vertico-repeat))
    :custom
    (vertico-cycle t)
    (vertico-mouse-mode t)
    (vertico-count 8)
    (vertico-count 8))

(use-package consult
    :straight t
    :bind (("C-x b" . consult-buffer)
           ("C-x C-b" . ibuffer)
           ("C-s" . consult-line)
           ("C-S-s" . consult-ripgrep)))

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

(use-package orderless
    :straight t
    :custom
    (completion-styles '(orderless basic))
    (completion-category-defaults nil)
    (completion-category-overrides '((file (styles basic partial-completion)))))

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
    :after (vertico)
    :bind ("<f7>" . consult-yasnippet))

(use-package projectile
    :straight t
    :bind-keymap ("C-c p" . projectile-command-map)
    :config
    (projectile-mode t)
    :custom
    (projectile-completion-system 'default))

(use-package flycheck
    :straight t
    :config
    (global-flycheck-mode))

(use-package magit
    :straight t)

(when (>= emacs-major-version 29)
    (setq major-mode-remap-alist
          '((python-mode . python-ts-mode))))

(use-package markdown-mode
    :straight t
    :mode (("README\\.md\\'" . gfm-mode)
           ("\\.md\\'" . markdown-mode)
           ("\\.markdown\\'" . markdown-mode))
    :custom
    (markdown-fontify-code-blocks-natively t)
    (markdown-command "multimarkdown"))

(defun my/angle-brackets-fix ()
    (modify-syntax-entry ?< "." org-mode-syntax-table)
    (modify-syntax-entry ?> "." org-mode-syntax-table))

(use-package org
    :straight t
    :hook ((org-mode . org-indent-mode)
           (org-mode . my/angle-brackets-fix))
    :config
    (org-babel-do-load-languages
     'org-babel-load-languages '((emacs-lisp . t)
                                 (python . t)
                                 (lua . t)
                                 (haskell . t)
                                 (shell . t)))
    :config
    (require 'org-tempo)
    (progn
        (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
        (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
        (add-to-list 'org-structure-template-alist '("hs" . "src haskell"))
        (add-to-list 'org-structure-template-alist '("lua" . "src lua"))
        (add-to-list 'org-structure-template-alist '("py" . "src python"))
        (add-to-list 'org-structure-template-alist '("tex" . "src tex")))
    :custom
    (org-edit-src-content-indentation 0)
    (org-src-preserve-indentation nil)
    (org-src-fontify-natively t)
    (org-src-tab-acts-natively t)
    (org-return-follows-link t)
    (org-mouse-1-follows-link t)
    (org-descriptive-links t)
    (org-hide-emphasis-markers t)
    (org-support-shift-select t))

(use-package edit-indirect
    :straight t)

(use-package org-bullets
    :straight t
    :after org
    :hook (org-mode . org-bullets-mode)
    :custom
    (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package toc-org
    :straight t
    :after org
    :hook (org-mode . toc-org-mode))

(use-package org-appear
    :straight (org-appear :type git :host github :repo "awth13/org-appear")
    :after org
    :hook (org-mode . org-appear-mode)
    :custom
    (org-appear-autolinks t)
    (org-appear-autosubmarkers t))

(use-package ess
    :straight t
    :mode (("\\.R$" . ess-r-mode)
           ("\\.do$" . ess-stata-mode))
    :hook ((ess-r-mode . lsp/lsp)
           (ess-r-post-run . ess-rdired)
           ((ess-r-mode ess-stata-mode) . (lambda ()
                                              (setq-local fill-column 80)
                                              (display-fill-column-indicator-mode))))
    :init
    (unless (getenv "LC_ALL")
        (setenv "LC_ALL" "ru_RU.UTF-8"))
    (setq display-buffer-alist
          (append `(("^\\*R Dired"
                     (display-buffer-reuse-window display-buffer-in-side-window)
                     (side . right)
                     (slot . -1)
                     (window-width . 0.33)
                     (reusable-frames . nil))
                    ("^\\*R view"
                     (display-buffer-reuse-window display-buffer-in-side-window)
                     (side . right)
                     (slot . 1)
                     (window-width . 0.33)
                     (reusable-frames . nil))
                    ("^\\*R"
                     (display-buffer-reuse-window display-buffer-in-side-window)
                     (side . right)
                     (slot . 1)
                     (window-width . 0.33)
                     (reusable-frames . nil)))
                  display-buffer-alist)))

(defun capf/python-mode ()
    "Extra CAPF for `python-mode'."
    (setq completion-at-point-functions
          (append completion-at-point-functions
                  (list 'cape-file))))

(defalias 'capf/python-ts-mode 'capf/python-mode)

(use-package python
    :straight lsp-pyright
    :hook (((python-mode python-ts-mode) . lsp/lsp)
           ((python-mode python-ts-mode) . (lambda ()
                                               (setq-local fill-column 80)
                                               (display-fill-column-indicator-mode)))))

(use-package js
    :mode "\\.js.R$"
    :hook (js-mode . lsp/lsp))

(use-package lua-mode
    :straight t
    :mode "\\.lua$"
    :custom
    (lua-indent-level 4))

(defun capf/latex-mode ()
    "Extra CAPF for `LaTeX-mode'."
    (progn
        (fset 'cape/company-reftex-labels
              (cape-company-to-capf #'company-reftex-labels))
        (fset 'cape/company-reftex-citations
              (cape-company-to-capf #'company-reftex-citations))
        (fset 'cape/company-math-symbols-latex
              (cape-company-to-capf #'company-math-symbols-latex))
        (fset 'cape/company-math-symbols-unicode
              (cape-company-to-capf #'company-math-symbols-unicode))
        (setq completion-at-point-functions
              (append completion-at-point-functions
                      (list 'cape/company-reftex-labels
                            'cape/company-reftex-citations
                            'cape/company-math-symbols-latex
                            'cape/company-math-symbols-unicode)))))

(defun auctex/non-greedy-capf ()
    "Making AUCTeX capf non-greedy."
    (progn
        (fset 'non-greedy-tex
              (cape-capf-properties #'TeX--completion-at-point :exclusive 'no))
        (setq completion-at-point-functions
              (list 'non-greedy-tex))))

(defun auctex/extra-commands ()
    "Add a command for TeX-file compilation via latexmk."
    (add-to-list
     'TeX-command-list
     '("LaTeX Make / PDFLaTeX"
       "latexmk -pdf -cd -f -interaction=nonstopmode -synctex=1 -shell-escape -outdir=output %t"
       TeX-run-TeX nil t
       :help "Make the file using Latexmk/PDFLaTeX."))
    (add-to-list
     'TeX-command-list
     '("LaTeX Make / XeLaTeX"
       "latexmk -pdfxe -cd -f -interaction=nonstopmode -synctex=1 -shell-escape -outdir=output %t"
       TeX-run-TeX nil t
       :help "Make the file using XeTeX."))
    (add-to-list
     'TeX-command-list
     '("LaTeX Make / LuaLaTeX"
       "latexmk -pdflua -cd -f -interaction=nonstopmode -synctex=1 -shell-escape -outdir=output %t"
       TeX-run-TeX nil t
       :help "Make the file using LuaTeX.")))

(defun my/region-or-env-or-paragraph ()
    "Produce region from LaTeX environment or paragraph if no any already."
    (unless (region-active-p)
        (if (equal major-mode 'latex-mode)
                (LaTeX-mark-environment)
            (mark-paragraph))
        (let ((beg (save-excursion
                       (goto-char (region-beginning))
                       (forward-line)
                       (line-beginning-position)))
              (end (if (equal major-mode 'latex-mode)
                           (save-excursion
                               (goto-char (region-end))
                               (forward-line (if (equal (point) (line-end-position))
                                                     -1
                                                 -2))
                               (line-end-position))
                       (region-end))))
            (set-mark beg)
            (goto-char end)
            (setq deactivate-mark nil))))

(defun my/region-expand-one-char ()
    "Add extra char to the end of region if possible."
    (if (and (= (region-end) (line-end-position))
             (/= (region-end) (line-beginning-position))
             (/= (region-end) (point-max)))
            (1+ (region-end))
        (region-end)))

(defun my/point-add-one-char (end)
    "Add new line if END is the last char and not at line-beginning."
    (interactive "r")
    (save-excursion
        (goto-char end)
        (if (and (= end (point-max))
                 (= end (line-end-position))
                 (/= end (line-beginning-position)))
                (insert "\n"))))

(defun my/protect-inner-amps ()
    "Protect ampersands in curly brackets."
    (let ((pos (point-min))
          (innerno 0))
        (while (< pos (point-max))
            (goto-char pos)
            (pcase (string (char-after pos))
                ("{" (setq innerno (1+ innerno)))
                ("}" (setq innerno (1- innerno)))
                ("&" (if (> innerno 0) (progn
                                           (delete-char 1)
                                           (insert "@")))))
            (setq pos (1+ pos)))
        (goto-char (point-min))
        (while (search-forward-regexp "\\\\&" nil t)
            (replace-match "\\\\@" nil nil))))

(defun my/unprotect-inner-amps ()
    "Restore protected ampersands."
    (goto-char (point-min))
            (while (search-forward "@" nil t)
                (replace-match "&" nil nil)))

(defun auctex/table-format (delim)
    "Convert table delimited by DELIM (usually copy-pasted from Excel)
to the LaTeX table."
    (interactive "sEnter delimiter (TAB by default): ")
    (when (string= delim "")
        (setq delim "\t"))
    (save-excursion
        (save-restriction
            (my/region-or-env-or-paragraph)
            (my/point-add-one-char (region-end))
            (narrow-to-region
             (region-beginning)
             (my/region-expand-one-char))
            (goto-char (point-min))
            (while (search-forward-regexp delim nil t)
                (replace-match " & " nil nil))
            (goto-char (point-min))
            (while (search-forward-regexp "\n" nil t)
                (replace-match " \\\\\\\\\n" nil nil)))))

(defun auctex/table-align ()
    "Align LaTeX table by its inner delimeters."
    (interactive)
    (save-excursion
        (save-restriction
            (my/region-or-env-or-paragraph)
            (my/point-add-one-char (region-end))
            (narrow-to-region
             (region-beginning)
             (my/region-expand-one-char))
            (my/protect-inner-amps)
            (goto-char (point-min))
            (while (search-forward-regexp "^&[ ]*" nil t)
                (replace-match " & " nil nil))
            (goto-char (point-min))
            (while (search-forward-regexp "[ ]*&[ ]*" nil t)
                (replace-match " & " nil nil))
            (align-regexp (point-min) (point-max) "\\(\\s-*\\)[^\\]&"
                          1 1 t)
            (align-regexp (point-min) (point-max) "\\(\\s-*\\)\\\\\\\\"
                          1 1 t)
            (goto-char (point-min))
            (my/unprotect-inner-amps))))

(use-package company-reftex
    :straight t)
(use-package company-auctex
    :straight t)
(use-package company-math
    :straight t)

(use-package LaTeX
    :straight auctex
    :hook ((LaTeX-mode . lsp/lsp)
           (LaTeX-mode . auctex/extra-commands)
           (LaTeX-mode . turn-on-reftex))
    :config
    (with-eval-after-load 'reftex
        (add-to-list 'reftex-section-levels
                     '("frametitle" . -2))
        (add-to-list 'reftex-section-levels
                     '("framesubtitle" . -3)))
    :custom
    (preview-pdf-color-adjust-method t)
    (preview-auto-cache-preamble t)
    (bibtex-dialect 'biblatex)
    (reftex-cite-format '((?\C-m . "\\cite[]{%l}")
                          (?a . "\\autocite[]{%l}")
                          (?p . "\\parencite[]{%l}")
                          (?f . "\\footcite[][]{%l}")
                          (?t . "\\textcite[]{%l}")
                          (?o . "\\citepr[]{%l}")
                          (?F . "\\fullcite[]{%l}")
                          (?n . "\\nocite{%l}")))
    (reftex-cite-prompt-optional-args t)
    (LaTeX-reftex-cite-format-auto-activate nil)
    (reftex-plug-into-AUCTeX t))
