;; early-init.el --- Emacs config -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Файл с настройками, применяемыми на раннем этапе, т. е. до основной инициализации.

;;; Code:
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

(setq user-emacs-directory
      (expand-file-name "emacs/" (or (getenv "XDG_CACHE_HOME") "~/.cache/")))

(setq native-comp-eln-load-path
      `(,(expand-file-name "eln-cache/" (or (getenv "XDG_CACHE_HOME") "~/.cache/"))))

(push (expand-file-name "modules/" (file-name-directory user-init-file))
      load-path)

(setq custom-file
      (expand-file-name "custom.el" user-emacs-directory))


;; Настройки UI
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

(setq visible-bell t
      ring-bell-function 'ignore
      inhibit-splash-screen t
      inhibit-startup-message t
      use-dialog-box nil)


;; Управление пакетами
(setq package-enable-at-startup nil
      package-quickstart nil)

(setq native-comp-speed -1)
(setq straight-check-for-modifications '(check-on-save find-when-checking))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
    (unless (file-exists-p bootstrap-file)
        (with-current-buffer
                (url-retrieve-synchronously
                 "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
                 'silent 'inhibit-cookies)
            (goto-char (point-max))
            (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))

;;; early-init.el ends here
