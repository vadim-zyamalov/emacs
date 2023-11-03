;; early-init.el --- Emacs config -*- lexical-binding: t; no-byte-compile: t; -*-

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'after-init-hook #'(lambda ()
                                 (setq gc-cons-threshold (* 8 1024 1024))
                                 (setq file-name-handler-alist file-name-handler-alist-original)
                                 (makunbound 'file-name-handler-alist-original)))

(setq user-emacs-directory
      (expand-file-name "emacs/" (or (getenv "XDG_CACHE_HOME") "~/.cache/")))

(setq native-comp-eln-load-path
      `(,(expand-file-name "eln-cache/" (or (getenv "XDG_CACHE_HOME") "~/.cache/"))))

(push (expand-file-name "modules/" (file-name-directory user-init-file))
      load-path)

(setq custom-file
      (expand-file-name "custom.el" user-emacs-directory))

(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

(setq visible-bell t
      ring-bell-function 'ignore
      inhibit-splash-screen t
      inhibit-startup-message t
      use-dialog-box nil)

(setq package-enable-at-startup nil
      package-quickstart nil)

(setq native-comp-speed -1)
(setq straight-check-for-modifications '(check-on-save find-when-checking))
