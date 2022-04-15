;; init.el --- Emacs config -*- lexical-binding: t; -*-

;;; Commentary:
;; Мой модульный файл настроек для Emacs.
;; Ранее использовал Org-файл для настроек, но его удобство,
;; заключающееся в возможности написания кода вместе с его описанием,
;; одновременно является и его недостатком.
;; Большой файл с настройками постепенно становится тяжело читаемым.

;;; Code:
(require 'module-setup)
(require 'module-base)
(require 'module-ui)
(require 'module-help)
(require 'module-themes)
(require 'module-input)
(require 'module-completion)
(require 'module-edit)
(require 'module-programming)

;;; init.el ends here
