;;; module-ui.el --- UI -*- lexical-binding: t; -*-

;;; Commentary:

;; Настройки интерфейса.
;; Данные настройки охватывают основной внешний вид фрейма Emacs.

;; https://gitlab.com/protesilaos/pulsar
;; https://github.com/emacs-dashboard/emacs-dashboard
;; https://www.emacswiki.org/emacs/TabBarMode
;; https://github.com/tarsius/minions
;; https://github.com/TeMPOraL/nyan-mode
;; https://github.com/cyrus-and/zoom
;; https://github.com/gonewest818/dimmer.el
;; https://github.com/jaypei/emacs-neotree

;;; Code:

(setq frame-resize-pixelwise t)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq split-width-threshold 80)

(setq-default cursor-type 'bar)


;; Вкладки
(setup tab-line
    (:global "M-<left>" previous-buffer
             "M-<right>" next-buffer)
    (global-tab-line-mode t))


;; Стартовый экран
(setup (:straight dashboard)
    (:straight all-the-icons)
    (:require all-the-icons)
    (:option dashboard-set-file-icons t
             dashboard-set-navigator t
             dashboard-items '((recents . 15)
                               (projects . 5))
             dashboard-startup-banner (expand-file-name
                                       "emacs.png"
                                       (file-name-directory user-init-file))
             dashboard-set-heading-icons t
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
                )))
    (dashboard-setup-startup-hook))


;; Строки
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)

(setup visual-line
    (:option visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
    (global-visual-line-mode 1))

(setup (:straight (pulsar :type git :host gitlab :repo "protesilaos/pulsar"))
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


;; Статусная строка
(setup (:straight minions)
    (minions-mode t))

(setup (:straight nyan-mode)
    (nyan-mode))

(setup (:straight doom-modeline)
    (:option doom-modeline-height 20
             doom-modeline-minor-modes t)
    (:with-hook after-init
        (:hook doom-modeline-mode)))


;; Окна
(setup (:straight zoom)
    (:option zoom-size '(0.618 . 0.618))
    (zoom-mode))

(setup (:straight dimmer)
    (:option dimmer-fraction 0.6
             dimmer-watch-frame-focus-events nil)
    (dimmer-configure-which-key)
    (add-to-list 'dimmer-buffer-exclusion-regexps "^.*\\*corfu\\*.*$")
    (dimmer-mode t))


;; Дерево каталогов
(setup (:straight neotree)
    (:option neo-smart-open t
             neo-window-width 35
             neo-theme (if (display-graphic-p) 'icons 'arrow))
    (:global "C-x t t" neotree-toggle))

(provide 'module-ui)
;;; module-ui.el ends here
