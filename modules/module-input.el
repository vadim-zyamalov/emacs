;;; module-input.el --- Input -*- lexical-binding: t; -*-

;;; Commentary:

;; Настройки мыши и клавиатуры: скроллинг, некоторые комбинации клавиш.
;; Особенно важно использование Cua-mode для повышения удобства путем
;; использования "стандартных" комбинаций клавиш.
;; Reverse-IM также важен для тех, что работает в нескольких клавиатурных
;; раскладках.

;; https://github.com/a13/reverse-im.el
;; https://www.emacswiki.org/emacs/CuaMode

;;; Code:

;; Мышь
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


;; Клавиатура
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
    (transient-mark-mode 1))

(provide 'module-input)
;;; module-input.el ends here
