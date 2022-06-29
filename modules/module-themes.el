;;; module-themes.el --- Themes -*- lexical-binding: t; -*-

;;; Commentary:

;; Настройки тем и красот: шрифт и цветовая схема.
;; А также поддержка лигатур

;; https://github.com/doomemacs/themes
;; https://www.jetbrains.com/lp/mono/
;; https://github.com/JetBrains/JetBrainsMono
;; https://github.com/domtronn/all-the-icons.el
;; https://github.com/iyefrat/all-the-icons-completion
;; https://github.com/mickeynp/ligature.el

;;; Code:

;; Тема
(setup (:straight doom-themes
                  solaire-mode)
    (:option doom-themes-enable-bold t
             doom-themes-enable-italic t)
    (doom-themes-visual-bell-config)
    (doom-themes-neotree-config)
    (doom-themes-org-config)
    (load-theme 'doom-palenight t)
    (solaire-global-mode t))

;; (setup modus-themes
;;     (:option modus-themes-mode-line '(borderless)
;;              modus-themes-bold-constructs t
;;              modus-themes-italic-constructs t)
;;     (load-theme 'modus-vivendi t))


;; Шрифты
(set-face-attribute 'default
                    nil
                    :font "JetBrains Mono"
                    :height 100)

;; Поддержка лигатур
;; Для шрифтов без их поддержки неактуально
(unless (version< emacs-version "28.1")
    (setup (:straight (ligature :type git :host github :repo "mickeynp/ligature.el"))
        (ligature-set-ligatures 'prog-mode '("-|" "-~" "---" "-<<" "-<" "--" "->" "->>" "-->" "///" "/=" "/=="
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
        (global-ligature-mode t)))


;; Иконки
(when (display-graphic-p)
    (progn
        (setup (:straight all-the-icons))
        (setup (:straight all-the-icons-completion)
            (:load-after marginalia all-the-icons)
            (:with-hook marginalia-mode-hook
                (:hook all-the-icons-completion-marginalia-setup))
            (all-the-icons-completion-mode))))

(provide 'module-themes)
;;; module-themes.el ends here
