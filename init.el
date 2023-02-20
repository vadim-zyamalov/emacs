;; init.el --- Emacs config -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Точка входа для основной настройки Emacs.
;; Сначала определяем используемые пакеты для автодополнений.
;; Затем запускаем последнюю версию org-mode.
;; После чего загружаем настройки из org-документа.

;;; Code:
(defconst init/lsp-engine "lsp"
         "LSP engine to use.")

(defconst init/completion-popup "corfu"
         "Completion popup to use.")

(defconst init/completion-minibuf "vertico"
         "Completion in minibuffer selector to use.")

(defconst init/snippet-engine "yasnippet"
         "Snippet engine to use.")

;; Гарантируем запуск последней установленной версии org-mode
(straight-use-package 'org)

;; Загружаем настройки из org-файла
(org-babel-load-file (concat (file-name-directory user-init-file) "config.org"))

;;; init.el ends here
