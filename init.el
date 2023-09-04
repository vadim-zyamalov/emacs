;; init.el --- Emacs config -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Точка входа для основной настройки Emacs.
;; Сначала определяем используемые пакеты для автодополнений.
;; Затем запускаем последнюю версию org-mode.
;; После чего загружаем настройки из org-документа.

;;; Code:
(defconst init/lsp-mode t
    "Use LSP-mode or Eglot otherwise.")

(defconst init/corfu t
    "Use corfu for buffer completion.")

(defconst init/vertico t
    "Use vertico for minibuffer completion.")

(defconst init/evil nil
    "To be evil or not.")

;; Гарантируем запуск последней установленной версии org-mode
(straight-use-package 'org)

;; Загружаем настройки из org-файла
(org-babel-load-file (concat (file-name-directory user-init-file) "config-use.org"))

;;; init.el ends here
