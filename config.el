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

(defconst ensure/is64
    (not (null
          (string-match "^x86_64-.*" system-configuration)))
    "Equals t if Emacs works on 64-bit system.")

(defconst ensure/isWindows
    (memq system-type '(cygwin windows-nt ms-dos))
    "Equals t if Emacs works on Windows host system.")

(prefer-coding-system 'utf-8)
(set-language-environment "utf-8")

(fset 'yes-or-no-p 'y-or-n-p)
(setq confirm-kill-emacs 'y-or-n-p)

(setq vc-follow-symlinks t)

(setup saveplace
    (:option save-place-file (expand-file-name
                              (format "%s/var/%s"
                                      user-emacs-directory
                                      "save-place.el")))
    (save-place-mode t))

(setup savehist
    (:option history-delete-duplicates t
             savehist-file (expand-file-name
                            (format "%s/var/%s"
                                    user-emacs-directory
                                    "savehist.el")))
    (savehist-mode t))

(add-hook 'emacs-startup-hook (lambda ()
                                  (setq gc-cons-threshold (* 8 1024 1024))
                                  (setq file-name-handler-alist file-name-handler-alist-original)
                                  (makunbound 'file-name-handler-alist-original)))

(setup (:straight no-littering)
    (setq auto-save-file-name-transforms
          `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

(setq create-lockfiles nil)

(add-hook 'before-save-hook
          (lambda ()
              (when buffer-file-name
                  (let ((dir (file-name-directory buffer-file-name)))
                      (when (and (not (file-exists-p dir))
                                 (y-or-n-p (format "Directory %s does not exist. Create it? " dir)))
                          (make-directory dir t))))))

(unless ensure/isWindows
    (add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p))

(setup dired
    (:option dired-recursive-deletes 'top))

(setq frame-resize-pixelwise t)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq split-width-threshold 80)

(setq-default cursor-type 'bar)

(setup tab-line
    (:global "M-<left>" previous-buffer
             "M-<right>" next-buffer)
    (global-tab-line-mode t))

(setup (:straight doom-modeline)
    (:option doom-modeline-height 24
             doom-modeline-minor-modes t)
    (:with-hook after-init-hook
        (:hook doom-modeline-mode)))

(setup (:straight minions)
    (minions-mode t))

(setup (:straight nyan-mode)
    (nyan-mode))

(setup (:straight dashboard)
    (:eval-after all-the-icons
        (:option dashboard-set-heading-icons t
                 dashboard-set-file-icons t
                 dashboard-items '((recents . 15)
                                   (projects . 5))
                 dashboard-startup-banner (expand-file-name
                                           "emacs.png"
                                           (file-name-directory user-init-file))
                 dashboard-set-navigator t
                 dashboard-navigator-buttons
                 `((
                    (,(all-the-icons-fileicon "emacs" :height 1.0 :v-adjust 0.0)
                     "Настройки"
                     "Открыть файл с настройками (init.el)"
                     (lambda (&rest _)
                         (find-file user-init-file)))
                    (,(all-the-icons-octicon "mark-github" :height 1.0 :v-adjust 0.0)
                     "dotfiles"
                     "Github с конфигурационными файлами"
                     (lambda (&rest _) (browse-url "https://github.com/d9d6ka/dotfiles")))
                    (,(all-the-icons-octicon "mark-github" :height 1.0 :v-adjust 0.0)
                     "emacs"
                     "Github с настройками Emacs"
                     (lambda (&rest _) (browse-url "https://github.com/d9d6ka/emacs")))
                    ))))
    (dashboard-setup-startup-hook))

(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)

(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
(global-visual-line-mode t)

(setup (:straight pulsar)
    (:option pulsar-pulse t
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
    (pulsar-global-mode t))

(setup (:straight zoom)
    (:option zoom-size '(0.618 . 0.618)
             zoom-ignored-major-modes '(ess-r-mode
                                        inferior-ess-r-mode
                                        ess-rdired-mode)
             zoom-ignored-buffer-names '("*R*"
                                         "*R dired*"
                                         "*R view*"))
    (zoom-mode))

(setup (:straight dimmer)
    (:option dimmer-fraction 0.6
             dimmer-watch-frame-focus-events nil)
    (dimmer-configure-which-key)
    (add-to-list 'dimmer-buffer-exclusion-regexps "^.*\\*corfu\\*.*$")
    (dimmer-mode t))

(setup (:straight framemove)
    (:option framemove-hook-into-windmove t)
    (:eval-after hydra
        (defhydra hydra-wind (global-map "<f6>")
            "Moving between windows"
            ("<left>"  windmove-left  "left")
            ("<right>" windmove-right "right")
            ("<up>"    windmove-up    "up")
            ("<down>"  windmove-down  "down"))))

(setup (:straight neotree)
    (:option neo-smart-open t
             neo-window-width 40
             neo-theme (if (display-graphic-p) 'icons 'arrow))
    (:global "C-x t t" neotree-toggle))

(setup (:straight doom-themes
                  solaire-mode)
    (:option doom-themes-enable-bold t
             doom-themes-enable-italic t)
    (doom-themes-visual-bell-config)
    (doom-themes-neotree-config)
    (doom-themes-org-config)
    (load-theme 'doom-palenight t)
    (solaire-global-mode t))

(set-face-attribute 'default
                    nil
                    :font "Fira Code"
                    :height 100)

(unless (version< emacs-version "28.1")
    (setup (:straight (ligature :type git :host github :repo "mickeynp/ligature.el"))
        (ligature-set-ligatures 'prog-mode (pcase (face-attribute 'default :family)
                                               ("JetBrains Mono" '("-|" "-~" "---" "-<<" "-<" "--" "->" "->>" "-->" "///" "/=" "/=="
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
                                               ((or "Fira Code" "Cascadia Code") '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
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
                                               ("Iosevka" '("<---" "<--"  "<<-" "<-" "->" "-->" "--->"
                                                            "<->" "<-->" "<--->" "<---->" "<!--" "<==" "<==="
                                                            "<=" "=>" "=>>" "==>" "===>" ">=" "<=>"
                                                            "<==>" "<===>" "<====>" "<!---" "<~~" "<~" "~>"
                                                            "~~>" "::" ":::" "==" "!=" "===" "!=="
                                                            ":=" ":-" ":+" "<*" "<*>" "*>" "<|"
                                                            "<|>" "|>" "+:" "-:" "=:" "<******>" "++"
                                                            "+++"))))
        (global-ligature-mode t)))

(setup (:straight all-the-icons
                  all-the-icons-completion)
    (all-the-icons-completion-mode))

(setup (:straight marginalia)
    (:eval-after all-the-icons-completion
        (:hook all-the-icons-completion-marginalia-setup))
    (marginalia-mode))

(setup (:straight which-key)
    (:option which-key-idle-delay 1)
    (which-key-mode))

(setup (:straight helpful)
    (:global [remap describe-function] helpful-callable
             "<f1> f" helpful-callable
             [remap describe-variable] helpful-variable
             "<f1> v" helpful-variable
             [remap describe-key] helpful-key
             "C-h C" helpful-command))

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

(setup (:straight hydra)
       (require 'hydra))

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

(setup (:straight reverse-im)
    (:option reverse-im-input-methods '("russian-computer"))
    (reverse-im-mode t))

(setup cua
    (:option cua-keep-region-after-copy t)
    (cua-mode t)
    (transient-mark-mode t))

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

(setup (:straight undo-tree)
    (global-undo-tree-mode)
    (:with-map global-map
        (:unbind "C-z"
                 "C-_"
                 "C-M-_")
        (:bind "C-z" undo-tree-undo
               "C-S-z" undo-tree-redo))
    (:bind-into cua--cua-keys-keymap
        "C-z" undo-tree-undo))

(show-paren-mode t)

(setup (:straight rainbow-delimiters)
    (:hook-into prog-mode org-mode))

(if (not (string-equal init/completion-popup "company"))
        (setup (:straight smartparens)
            (:require smartparens-config)
            (:bind "C-c b r" sp-rewrap-sexp
                   "C-c b d" sp-splice-sexp)
            (smartparens-global-mode t)
            (sp-with-modes '(tex-mode
                             latex-mode
                             LaTeX-mode)
                (sp-local-pair "<<" ">>"
                               :unless '(sp-in-math-p))))
    (electric-pair-mode t))

(defun comment-or-uncomment-region-or-line ()
    "Comments or uncomments the region or the current line."
    (interactive)
    (let (beg end)
        (if (region-active-p)
                (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)
        (forward-line)))

(global-set-key (kbd "M-;") 'comment-or-uncomment-region-or-line)

(setup (:straight visual-regexp)
    (:require visual-regexp)
    (:global "M-%" vr/replace
             "C-M-%" vr/query-replace
             "C-c v m" vr/mc-mark))

(setup (:straight multiple-cursors)
    (:option mc/match-cursor-style nil)
    (:global "C-c m l" mc/edit-lines
             "C->" mc/mark-next-like-this
             "C-<" mc/mark-previous-like-this
             "C-c m a" mc/mark-all-like-this))

(setup (:straight crux)
    (:require crux)
    (:bind-into global-map
        "C-c I" crux-find-user-init-file
        "C-c d" crux-duplicate-current-line-or-region
        "C-c M-d" crux-duplicate-and-comment-current-line-or-region
        "S-<return>" crux-smart-open-line
        "C-S-<return>" crux-smart-open-line-above))

(setup (:straight cape))

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

(when (string-equal init/lsp-engine "lsp")
    (setup (:straight lsp-mode)
        (:option lsp-headerline-breadcrumb-icons-enable nil
                 lsp-enable-file-watchers nil
                 lsp-keymap-prefix "C-c l")
        (if (equal init/completion-popup "corfu")
                (:option lsp-completion-provider :none))
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

(when (string-equal init/completion-popup "corfu")
    (setup (:straight corfu
                      corfu-doc
                      kind-icon
                      popon
                      corfu-terminal
                      (corfu-doc-terminal
                       :type git
                       :repo "https://codeberg.org/akib/emacs-corfu-doc-terminal.git"))
        (:option corfu-auto nil
                 corfu-cycle t
                 corfu-preselect-first nil
                 corfu-preview-current 'insert
                 tab-always-indent 'complete
                 kind-icon-default-face 'corfu-default)
        (:hook corfu-doc-mode)
        (:bind-into corfu-map
            "TAB" corfu-next
            [tab] corfu-next
            "S-TAB" corfu-previous
            [backtab] corfu-previous)
        (global-corfu-mode)
        (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
        (add-to-list 'completion-at-point-functions #'cape-file t)
        (unless (display-graphic-p)
            (corfu-terminal-mode t)
            (corfu-doc-terminal-mode t))))

(when (string-equal init/completion-popup "company")
    (setup (:straight company
                      company-box)
        (:option tab-always-indent 'complete
                 company-backends '((company-capf))
                 company-selection-wrap-around t
                 company-minimum-prefix-length 1
                 company-idle-delay nil
                 company-tooltip-align-annotations t
                 company-transformers '(delete-consecutive-dups
                                        company-sort-by-occurrence
                                        company-sort-prefer-same-case-prefix))
        (:global [remap indent-for-tab-command] company-indent-or-complete-common)
        (:bind-into company-active-map
            "<tab>" company-complete-common-or-cycle)
        (:hook company-box-mode)
        (global-company-mode)))

(when (equal init/completion-minibuf "vertico")
    (setup (:straight (vertico :files (:defaults "extensions/*")
                               :includes (vertico-mouse
                                          vertico-repeat))
                      consult
                      embark
                      orderless)
        (:option vertico-cycle t
                 vertico-mouse-mode t
                 vertico-count 8
                 vertico-resize t
                 prefix-help-command #'embark-prefix-help-command
                 completion-styles '(orderless basic)
                 completion-category-defaults nil
                 completion-category-overrides '((file (styles basic partial-completion))))
        (:global "C-x b" consult-buffer
                 "C-x C-b" ibuffer
                 "C-." embark-act
                 "C-;" embark-dwim
                 "C-h B" embark-bindings
                 "C-s" consult-line
                 "M-R" vertico-repeat)
        (:with-hook minibuffer-setup-hook
            (:hook (lambda ()
                       (setq completion-in-region-function
                             (if vertico-mode
                                     #'consult-completion-in-region
                                 #'completion--in-region)))
                   vertico-repeat-save))
        (vertico-mode)
        (:with-mode embark-collect-mode
            (:hook consult-preview-at-point-mode))))

(when (equal init/completion-minibuf "ivy")
    (setup (:straight ivy
                      swiper
                      counsel
                      smex)
        (:option ivy-use-virtual-buffers t
                 ivy-count-format "(%d/%d) "
                 ivy-wrap t)
        (:global "C-s" swiper-isearch
                 "M-x" counsel-M-x
                 "C-x C-f" counsel-find-file
                 "M-y" counsel-yank-pop
                 "<f1> l" counsel-find-library
                 "<f2> i" counsel-info-lookup-symbol
                 "<f2> u" counsel-unicode-char
                 "<f2> j" counsel-set-variable
                 "C-x b" ivy-switch-buffer
                 "C-x C-b" ibuffer
                 "C-c v" ivy-push-view
                 "C-c V" ivy-pop-view
                 "M-R" ivy-resume)
        (ivy-mode t)))

(when (string-equal init/snippet-engine "tempel")
    (setup (:straight tempel)
        (:option tempel-path (expand-file-name
                              "templates"
                              (file-name-directory user-init-file)))
        (:global "<backtab>" tempel-complete
                 "<f7>" tempel-insert)
        (:bind-into tempel-map
            "<backtab>" tempel-done)))

(when (string-equal init/snippet-engine "yasnippet")
    (setup (:straight yasnippet)
        (:option yas-snippet-dirs (append yas-snippet-dirs
                                          '("~/.emacs.d/snippets")))
        (yas-global-mode 1))

    (setup (:straight yasnippet-snippets))

    (setup (:straight consult-yasnippet)
        (:global "<f7>" consult-yasnippet)))

(setup (:straight projectile)
    (:option projectile-completion-system 'default)
    (:bind "C-c p" projectile-command-map)
    (projectile-mode t))

(setup (:straight flycheck)
    (global-flycheck-mode))

(setup (:straight markdown-mode)
    (:with-mode gfm-mode
        (:file-match "README\\.md\\'"))
    (:with-mode markdown-mode
        (:file-match "\\.md\\'"
                     "\\.markdown\\'"))
    (:option markdown-fontify-code-blocks-natively t
             markdown-command "multimarkdown"))

(defun my/angle-brackets-fix ()
    (modify-syntax-entry ?< "." org-mode-syntax-table)
    (modify-syntax-entry ?> "." org-mode-syntax-table))

(setup org
    (:straight edit-indirect
               org-bullets
               toc-org
               org-appear)
    (:option org-edit-src-content-indentation 0
             org-src-preserve-indentation nil
             org-src-fontify-natively t
             org-src-tab-acts-natively t
             org-return-follows-link t
             org-mouse-1-follows-link t
             org-descriptive-links t
             org-hide-emphasis-markers t
             org-support-shift-select t
             org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")
             org-appear-autolinks t
             org-appear-autosubmarkers t)
    (require 'org-tempo)
    (org-babel-do-load-languages
     'org-babel-load-languages '((emacs-lisp . t)
                                 (python . t)
                                 (lua . t)
                                 (haskell . t)
                                 (shell . t)))
    (progn
        (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
        (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
        (add-to-list 'org-structure-template-alist '("hs" . "src haskell"))
        (add-to-list 'org-structure-template-alist '("lua" . "src lua"))
        (add-to-list 'org-structure-template-alist '("py" . "src python"))
        (add-to-list 'org-structure-template-alist '("tex" . "src tex")))
    (:hook org-indent-mode
           my/angle-brackets-fix
           org-bullets-mode
           toc-org-mode
           org-appear-mode))

(setup (:straight ess
                  poly-R)
    (:option polymode-lsp-integration nil)
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
                  display-buffer-alist))
    (:with-mode ess-r-mode
        (setq-local fill-column 80)
        (:file-match "\\.R$")
        (:hook lsp/lsp
               (lambda ()
                   (setq-local fill-column 80)
                   (display-fill-column-indicator-mode)))
        (:with-hook ess-r-post-run-hook
            (:hook ess-rdired)))
    (:with-mode ess-stata-mode
        (:file-match "\\.do$")
        (:hook (lambda ()
                   (setq-local fill-column 80)
                   (display-fill-column-indicator-mode)))))

(defun capf/python-mode ()
    "Extra CAPF for `python-mode'."
    (setq completion-at-point-functions
          (append completion-at-point-functions
                  (list 'cape-file))))

(setup python
    (:straight lsp-pyright)
    (:option python-shell-interpreter "python3")
    (:hook lsp/lsp
           (lambda ()
               (setq-local fill-column 80)
               (display-fill-column-indicator-mode))))

(setup js
    (:file-match "\\.js.R$")
    (:hook lsp/lsp))

(setup (:straight lua-mode)
    (:file-match "\\.lua$")
    (:option lua-indent-level 4))

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

(defun auctex/latexmk ()
    "Add a command for TeX-file compilation via latexmk."
    (add-to-list
     'TeX-command-list
     '("LaTeX Make"
       "latexmk -pdf -cd -f -interaction=nonstopmode -synctex=1 -shell-escape -outdir=output %t"
       TeX-run-TeX nil t
       :help "Make the file using latexmk.")))

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
            (goto-char end))))

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

(defun auctex/table-format (delim)
    "Convert table delimited by DELIM (usually copy-pasted from Excel)
to the LaTeX table."
    (interactive "sEnter delimiter (TAB by default): ")
    (when (string= delim "")
        (setq delim "\t"))
    (save-restriction
        (save-excursion
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
    (save-restriction
        (save-excursion
            (my/region-or-env-or-paragraph)
            (my/point-add-one-char (region-end))
            (narrow-to-region
             (region-beginning)
             (my/region-expand-one-char))
            (goto-char (point-min))
            (while (search-forward-regexp "[ ]*&[ ]*" nil t)
                (replace-match " & " nil nil)))
        (align-regexp (point-min) (point-max) "\\(\\s-*\\)&"
                      1 1 t)
        (align-regexp (point-min) (point-max) "\\(\\s-*\\)\\\\\\\\"
                      1 1 t)))

(setup LaTeX
    (:straight auctex
               company-reftex
               company-auctex
               company-math)
    (:option preview-pdf-color-adjust-method t
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
    (:hook lsp/lsp
           auctex/latexmk
           turn-on-reftex))
