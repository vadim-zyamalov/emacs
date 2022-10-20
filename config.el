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
    (windmove-default-keybindings '(shift meta ctrl)))

(setup (:straight neotree)
    (:option neo-smart-open t
             neo-window-width 40
             neo-theme (if (display-graphic-p) 'icons 'arrow))
    (:global "C-x t t" neotree-toggle))
