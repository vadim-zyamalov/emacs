;; init.el --- Emacs config -*- lexical-binding: t; no-byte-compile: t; -*-

(defconst init/lsp-mode t
    "Use LSP-mode or Eglot otherwise.")

(defconst init/corfu t
    "Use corfu for buffer completion.")

(defconst init/vertico t
    "Use vertico for minibuffer completion.")

(defconst init/evil nil
    "To be evil or not.")

(defconst ensure/isWindows
    (memq system-type '(cygwin windows-nt ms-dos))
    "Equals t if Emacs works on Windows host system.")

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
    (unless (file-exists-p bootstrap-file)
        (with-current-buffer
                (url-retrieve-synchronously
                 "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
                 'silent 'inhibit-cookies)
            (goto-char (point-max))
            (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))

(when (< emacs-major-version 29)
    (straight-use-package 'use-package))

(require 'use-package)

(use-package use-package-hydra
    :straight t)

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
    :init
    (setq reverse-im-input-methods '("russian-computer"))
    :config
    (reverse-im-mode t))

(unless init/evil
    (setq cua-keep-region-after-copy t)
    (cua-mode t)
    (transient-mark-mode t))

(use-package hydra
    :straight t)

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

(setq require-final-newline t)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

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

(setq-default indent-tabs-mode nil
              tab-width 4
              c-basic-offset 4
              standart-indent 4
              lisp-body-indent 4)

(electric-indent-mode t)

(define-key global-map (kbd "RET") 'newline-and-indent)

(use-package indent-bars
    :straight (indent-bars
               :type git
               :host github
               :repo "jdtsmith/indent-bars")
    :init
    (setq indent-bars-prefer-character t
          indent-bars-treesit-support t
          indent-bars-no-descend-string nil
          indent-bars-treesit-ignore-blank-lines-types '("module")
          indent-bars-treesit-wrap '((python argument_list parameters ; for python, as an example
                                             list list_comprehension
                                             dictionary dictionary_comprehension
                                             parenthesized_expression subscript)))
    :hook (prog-mode . indent-bars-mode))

(use-package undo-tree
    :straight t
    :bind (("C-z" . undo-tree-undo)
           ("C-S-z" . undo-tree-redo)
           :map cua--cua-keys-keymap
           ("C-z" . undo-tree-undo))
    :init
    (unbind-key "C-z" global-map)
    (unbind-key "C-_" global-map)
    (unbind-key "C-M-_" global-map)
    (setq undo-tree-history-directory-alist `(("." . ,(format "%s/undo"
                                                              user-emacs-directory))))
    :config
    (global-undo-tree-mode))

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

(setq frame-resize-pixelwise t)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq split-width-threshold 80)

(setq-default cursor-type 'bar)

(use-package catppuccin-theme
    :straight t
    :init
    (setq catppuccin-flavor 'mocha)
    :config
    (load-theme 'catppuccin :no-confirm))

(cond ((find-font (font-spec :name "Cascadia Code"))
       (set-face-attribute 'default
                           nil
                           :font "Cascadia Code"
                           :height 120)
       (setq nerd-icons-font-family "Cascadia Code NF"))
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
             ((or "Fira Code" "Cascadia Code" "Cascadia Code NF")
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
    :after marginalia
    :config
    (nerd-icons-completion-mode)
    (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-dired
    :straight t
    :hook
    (dired-mode . nerd-icons-dired-mode))

(use-package tab-line
    :demand t
    :bind (("M-<left>" . previous-buffer)
           ("M-<right>" . next-buffer))
    :config
    (global-tab-line-mode t))

(use-package doom-modeline
    :straight t
    :init
    (setq doom-modeline-height 24
          doom-modeline-minor-modes t)
    :hook (after-init . doom-modeline-mode))

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
    :after nerd-icons
    :init
    (setq dashboard-display-icons-p t
          dashboard-icon-type 'nerd-icons
          dashboard-set-heading-icons t
          dashboard-set-file-icons t
          dashboard-items '((recents . 15)
                            (projects . 5))
          dashboard-startup-banner (expand-file-name
                                    "it-people.png"
                                    (file-name-directory user-init-file))
          dashboard-navigator-buttons
          `((
             (,(nerd-icons-sucicon "nf-custom-emacs" :height 1.0 :v-adjust 0.0)
              "Настройки"
              "Открыть файл с настройками (init.el)"
              (lambda (&rest _)
                  (find-file (concat (file-name-directory user-init-file) "init.org"))))
             (,(nerd-icons-faicon "nf-fa-github" :height 1.0 :v-adjust 0.0)
              "dotfiles"
              "Github с конфигурационными файлами"
              (lambda (&rest _) (browse-url "https://github.com/vadim-zyamalov/dotfiles")))
             (,(nerd-icons-faicon "nf-fa-github" :height 1.0 :v-adjust 0.0)
              "emacs"
              "Github с настройками Emacs"
              (lambda (&rest _) (browse-url "https://github.com/vadim-zyamalov/emacs")))
             ))
          dashboard-startupify-list '(dashboard-insert-banner
                                      dashboard-insert-newline
                                      dashboard-insert-banner-title
                                      dashboard-insert-newline
                                      dashboard-insert-navigator
                                      dashboard-insert-newline
                                      dashboard-insert-init-info
                                      dashboard-insert-items
                                      dashboard-insert-newline
                                      dashboard-insert-footer))
    :config
    (dashboard-setup-startup-hook))

(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)

(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
(global-visual-line-mode t)

(use-package pulsar
    :straight t
    :init
    (setq pulsar-pulse t
          pulsar-delay 0.055
          pulsar-pulse-functions '(recenter-top-bottom
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
                                   consult-line))
    :config
    (pulsar-global-mode t))

(use-package dimmer
    :straight t
    :init
    (setq dimmer-fraction 0.6
          dimmer-watch-frame-focus-events nil)
    :config
    (dimmer-configure-which-key)
    (add-to-list 'dimmer-buffer-exclusion-regexps "^.*\\*corfu\\*.*$")
    (add-to-list 'dimmer-buffer-exclusion-regexps "^.*\\*corfu-popupinfo\\*.*$")
    (dimmer-mode t))

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
    :init
    (setq framemove-hook-into-windmove t))

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

(use-package marginalia
    :straight t
    :init
    (marginalia-mode))

(use-package which-key
    :straight t
    :init
    (setq which-key-idle-delay 1)
    :config
    (which-key-mode))

(use-package helpful
    :straight t
    :bind (([remap describe-function] . helpful-callable)
           ("<f1> f" . helpful-callable)
           ([remap describe-variable] . helpful-variable)
           ("<f1> v" . helpful-variable)
           ([remap describe-key] . helpful-key)
           ("C-h F" . helpful-function)
           ("C-h C" . helpful-command)))

(use-package cape
    :straight t
    :config
    (add-to-list 'completion-at-point-functions #'cape-file t))

(defun lsp/lsp ()
    "Using an appropriate LSP-engine."
    (if init/lsp-mode
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

(when init/lsp-mode
    (use-package lsp-mode
        :straight t
        :init
        (setq lsp-headerline-breadcrumb-icons-enable nil
              lsp-enable-file-watchers nil
              lsp-keymap-prefix "C-c l"
              lsp-completion-provider :none)
        :hook ((lsp-mode . lsp-enable-which-key-integration)
               (lsp-completion-mode . (lambda ()
                                          (progn
                                              (lsp/non-greedy-lsp-mode)
                                              (lsp/extra-capf)))))
        :config
        (with-eval-after-load 'lsp-mode
            (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)))

    (use-package lsp-ui
        :straight t))

(unless init/lsp-mode
    (when (< emacs-major-version 29)
        (straight-use-package 'eglot))
    (use-package eglot
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

(when init/corfu
    (use-package corfu
        :straight (:files (:defaults "extensions/*"))
        :bind (:map corfu-map
                    ("TAB" . corfu-next)
                    ([tab] . corfu-next)
                    ("S-TAB" . corfu-previous)
                    ([backtab] . corfu-previous))
        :init
        (setq corfu-auto nil
              corfu-cycle t
              corfu-preselect-first nil
              corfu-preview-current 'insert
              tab-always-indent 'complete
              corfu-popupinfo-delay 0.2)
        (corfu-popupinfo-mode)
        (global-corfu-mode))

    (use-package kind-icon
        :straight t
        :after (corfu nerd-icons)
        :init
        (setq kind-icon-default-face 'corfu-default)
        :config
        (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)))

(unless init/corfu
    (use-package company
        :straight t
        :bind (([remap indent-for-tab-command] . company-indent-or-complete-common)
               :map company-active-map
               ("RET". company-complete-selection)
               ("<return>". company-complete-selection)
               ("<tab>" . company-complete-common-or-cycle)
               ("ESC" . company-abort)
               ("<esc>" . company-abort))
        :hook (after-init . global-company-mode)
        :init
        (setq company-backends '((company-capf))
              company-selection-wrap-around t
              company-minimum-prefix-length 1
              company-idle-delay nil
              company-tooltip-align-annotations t
              company-transformers '(delete-consecutive-dups
                                     company-sort-by-occurrence
                                     company-sort-prefer-same-case-prefix)))

    (use-package company-box
        :straight t
        :hook (company-mode . company-box-mode)))

(when init/vertico
    (use-package vertico
        :straight (:files (:defaults "extensions/*"))
        :hook ((minibuffer-setup . (lambda ()
                                       (setq completion-in-region-function
                                             (if vertico-mode
                                                     #'consult-completion-in-region
                                                 #'completion--in-region))))
               (minibuffer-setup . vertico-repeat-save))
        :init
        (setq vertico-cycle t
              vertico-mouse-mode t
              vertico-count 8
              vertico-count 8)
        (add-to-list 'process-coding-system-alist
                     '("[rR][gG]" . (utf-8-dos . windows-1251-dos)))
        (vertico-mode)
        :bind (("M-R" . vertico-repeat)))

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
        :init
        (setq prefix-help-command #'embark-prefix-help-command))

    (use-package embark-consult
        :straight t
        :after (embark consult)
        :hook (embark-collect-mode . consult-preview-at-point-mode))

    (use-package orderless
        :straight t
        :init
        (setq completion-styles '(orderless basic)
              completion-category-defaults nil
              completion-category-overrides '((file (styles basic partial-completion))))))

(unless init/vertico
    (use-package counsel
        :straight t
        :config
        (ivy-mode t)
        :bind (("C-x b"   . ivy-switch-buffer)
               ("C-x C-b" . ibuffer)
               ("C-c v"   . ivy-push-view)
               ("C-c V"   . ivy-pop-view)
               ("M-R"     . ivy-resume)
               ("C-s"     . swiper-isearch)
               ("M-x"     . counsel-M-x)
               ("C-x C-f" . counsel-find-file)
               ("M-y"     . counsel-yank-pop)
               ("<f1> l"  . counsel-find-library)
               ("<f2> i"  . counsel-info-lookup-symbol)
               ("<f2> u"  . counsel-unicode-char)
               ("<f2> j"  . counsel-set-variable))
        :init
        (setq ivy-use-virtual-buffers t
              ivy-count-format "(%d/%d) "
              ivy-wrap t))

    (use-package ivy-rich
        :straight t
        :after (counsel)
        :init
        (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
        :config
        (ivy-rich-mode 1))

    (use-package nerd-icons-ivy-rich
        :straight t
        :init
        (nerd-icons-ivy-rich-mode 1)))

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
    :init
    (setq projectile-completion-system 'default)
    :config
    (projectile-mode t))

(use-package magit
    :straight t)

(when (>= emacs-major-version 29)
    (setq major-mode-remap-alist
          '((python-mode . python-ts-mode))))

(use-package eshell-prompt-extras
    :straight t
    :init
    (autoload 'epe-theme-lambda "eshell-prompt-extras")
    (setq eshell-highlight-prompt nil
        eshell-prompt-function 'epe-theme-lambda))

(use-package markdown-mode
    :straight t
    :mode (("README\\.md\\'" . gfm-mode)
           ("\\.md\\'" . markdown-mode)
           ("\\.markdown\\'" . markdown-mode))
    :init
    (setq markdown-fontify-code-blocks-natively t
          markdown-command "multimarkdown"))

(defun my/angle-brackets-fix ()
    (modify-syntax-entry ?< "." org-mode-syntax-table)
    (modify-syntax-entry ?> "." org-mode-syntax-table))

(use-package org
    :straight t
    :hook ((org-mode . org-indent-mode)
           (org-mode . my/angle-brackets-fix))
    :init
    (setq org-fold-core-style 'overlays
          org-edit-src-content-indentation 0
          org-src-preserve-indentation nil
          org-src-fontify-natively t
          org-src-tab-acts-natively t
          org-return-follows-link t
          org-mouse-1-follows-link t
          org-descriptive-links t
          org-hide-emphasis-markers t
          org-support-shift-select t)
    :config
    (org-babel-do-load-languages
     'org-babel-load-languages '((emacs-lisp . t)
                                 (python . t)
                                 (lua . t)
                                 (haskell . t)
                                 (shell . t)))
    (require 'org-tempo)
    (progn
        (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
        (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
        (add-to-list 'org-structure-template-alist '("hs" . "src haskell"))
        (add-to-list 'org-structure-template-alist '("lua" . "src lua"))
        (add-to-list 'org-structure-template-alist '("py" . "src python"))
        (add-to-list 'org-structure-template-alist '("tex" . "src tex"))))

(use-package edit-indirect
    :straight t)

(use-package org-bullets
    :straight t
    :after org
    :hook (org-mode . org-bullets-mode)
    :init
    (setq org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package toc-org
    :straight t
    :after org
    :hook (org-mode . toc-org-mode))

(use-package org-appear
    :straight t
    :after org
    :hook (org-mode . org-appear-mode)
    :config
    (setq org-appear-trigger 'always
          org-appear-autolinks t
          org-appear-autosubmarkers t))

(use-package org-auto-tangle
    :straight t
    :hook (org-mode . org-auto-tangle-mode))

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
                                               (setq python-shell-interpreter "python")
                                               (display-fill-column-indicator-mode)))))

(use-package js
    :mode "\\.js.R$"
    :hook (js-mode . lsp/lsp))

(use-package lua-mode
    :straight t
    :mode "\\.lua$"
    :init
    (setq lua-indent-level 4))

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
    :after auctex
    :straight t)
(use-package company-auctex
    :after auctex
    :straight t)
(use-package company-math
    :after auctex
    :straight t)

(use-package LaTeX
    :straight auctex
    :hook ((LaTeX-mode . lsp/lsp)
           (LaTeX-mode . auctex/extra-commands)
           (LaTeX-mode . turn-on-reftex))
    :init
    (setq preview-pdf-color-adjust-method t
          preview-auto-cache-preamble t
          bibtex-dialect 'biblatex
          reftex-cite-format '((?\C-m . "\\cite[]{%l}")
                               (?a . "\\autocite[]{%l}")
                               (?p . "\\parencite[]{%l}")
                               (?f . "\\footcite[][]{%l}")
                               (?t . "\\textcite[]{%l}")
                               (?o . "\\citepr[]{%l}")
                               (?F . "\\fullcite[]{%l}")
                               (?n . "\\nocite{%l}"))
          reftex-cite-prompt-optional-args t
          LaTeX-reftex-cite-format-auto-activate nil
          reftex-plug-into-AUCTeX t)
    :config
    (with-eval-after-load 'reftex
        (add-to-list 'reftex-section-levels
                     '("frametitle" . -2))
        (add-to-list 'reftex-section-levels
                     '("framesubtitle" . -3))))
