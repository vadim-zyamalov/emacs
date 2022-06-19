;;; module-edit.el --- Editing -*- lexical-binding: t; -*-

;;; Commentary:

;; Пакеты, облегчающие редактирование файлов.
;; В данном файле приведены настройки всего, что касается ввода текста.
;; За исключением автодополнения, так как этот аспект настолько важен и самобытен,
;; что его целесообразно размещать в отдельном файле.

;; https://gitlab.com/ideasman42/emacs-undo-fu
;; https://gitlab.com/tsc25/undo-tree
;; https://github.com/Fuco1/smartparens
;; https://github.com/Fanael/rainbow-delimiters
;; https://github.com/benma/visual-regexp.el
;; https://github.com/magnars/multiple-cursors.el
;; https://stackoverflow.com/a/9697222

;;; Code:

(setq-default indent-tabs-mode nil
              tab-width 4
              c-basic-offset 4
              standart-indent 4
              lisp-body-indent 4)
(define-key global-map (kbd "RET") 'newline-and-indent)

(delete-selection-mode t)

(setq sentence-end-double-space nil)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq require-final-newline t)


;; Отмена/повтор
(setup (:straight undo-fu)
    (:require undo-fu)
    (:option undo-fu-allow-undo-in-region nil
             undo-fu-ignore-keyboard-quit t)
    (:with-map global-map
        (:unbind "C-z"
                 "C-_"
                 "C-M-_")
        (:bind "C-z" undo-fu-only-undo
               "C-S-z" undo-fu-only-redo))
    (:bind-into cua--cua-keys-keymap
        "C-z" undo-fu-only-undo))


;; Скобки
(show-paren-mode t)
;; (electric-pair-mode 1)
(electric-indent-mode t)

(setup (:straight (smartparens :type git :host github :repo "Fuco1/smartparens"))
    (:require smartparens-config)
    (:bind "C-c b r" sp-rewrap-sexp
           "C-c b d" sp-splice-sexp)
    (smartparens-global-mode t)
    (sp-with-modes '(tex-mode
                     latex-mode
                     LaTeX-mode)
        (sp-local-pair "<<" ">>"
                       :unless '(sp-in-math-p))))

(setup (:straight rainbow-delimiters)
    (:hook-into prog-mode org-mode))


;; Комментирование
(defun comment-or-uncomment-region-or-line ()
    "Comments or uncomments the region or the current line if there's no active region."
    (interactive)
    (let (beg end)
        (if (region-active-p)
                (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)
        (next-line)))

(global-set-key (kbd "M-;") 'comment-or-uncomment-region-or-line)


;; Поиск/замена
(setup (:straight ctrlf)
    (:option ctrlf-auto-recenter t)
    (ctrlf-mode t))

(setup (:straight visual-regexp)
    (:require visual-regexp)
    (:global "M-%" vr/replace
             "C-M-%" vr/query-replace
             "C-c v m" vr/mc-mark))


;; Мультикурсор
(setup (:straight multiple-cursors)
    (:option mc/match-cursor-style nil)
    (:global "C-c m l" mc/edit-lines
             "C->" mc/mark-next-like-this
             "C-<" mc/mark-previous-like-this
             "C-c m a" mc/mark-all-like-this))


;; Разные полезные команды
(setup (:straight crux)
    (:require crux)
    (:bind-into global-map
        "C-c I" crux-find-user-init-file
        "C-c d" crux-duplicate-current-line-or-region
        "C-c M-d" crux-duplicate-and-comment-current-line-or-region
        "S-<return>" crux-smart-open-line
        "C-S-<return>" crux-smart-open-line-above))

(provide 'module-edit)
;;; module-edit.el ends here
