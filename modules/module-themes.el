;;; module-themes.el --- Themes -*- lexical-binding: t; -*-

;;; Commentary:

;; Настройки тем и красот: шрифт и цветовая схема.
;; А также поддержка лигатур

;; https://github.com/doomemacs/themes
;; https://github.com/be5invis/Iosevka

;;; Code:

;; Тема
(setup (:straight doom-themes)
    (:option doom-themes-bold t
             doom-themes-enable-italic t)
    (doom-themes-visual-bell-config)
    ;; (doom-themes-treemacs-config)
    (doom-themes-neotree-config)
    (doom-themes-org-config)
    (load-theme 'doom-one t))


;; Шрифты
(set-face-attribute 'default
                    nil
                    :font "Fira Code"
                    :height 110)

(unless (version< emacs-version "28.1")
    (setup (:straight (ligature :type git :host github :repo "mickeynp/ligature.el"))
        (ligature-set-ligatures 'prog-mode '("<---" "<--" "<<-" "<-" "->" "-->" "--->"
                                             "<->" "<-->" "<--->" "<---->"
                                             "<!--" "<==" "<===" "<=" "=>" "=>>" "==>" "===>" ">="
                                             "<=>" "<==>" "<===>" "<====>" "<!---"
                                             "<~~" "<~" "~>" "~~>"
                                             "::" ":::"
                                             "==" "!=" "===" "!=="
                                             ":=" ":-" ":+" "+:" "-:" "=:"
                                             "<*" "<*>" "*>"
                                             "<|" "<|>" "|>"
                                             "<******>" "++" "+++"))
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
