;; init.el --- Emacs config -*- lexical-binding: t; -*-

;;; Commentary:
;; Мой модульный файл настроек для Emacs.
;; Ранее использовал Org-файл для настроек, но его удобство,
;; заключающееся в возможности написания кода вместе с его описанием,
;; одновременно является и его недостатком.
;; Большой файл с настройками постепенно становится тяжело читаемым.

;;; Code:
(defconst init/lsp-engine "lsp"
         "LSP engine to use.")

(defconst init/completion-popup "corfu"
         "Completion popup to use.")

(defconst init/snippet-engine "yasnippet"
         "Snippet engine to use.")

(require 'module-setup)
(require 'module-base)
(require 'module-ui)
(require 'module-themes)
(require 'module-help)
(require 'module-input)
(require 'module-edit)
(require 'module-completion)
(require 'module-programming)

;; (org-babel-load-file (concat (expand-file-name "~/.emacs.d/") "config.org"))

;;; init.el ends here
