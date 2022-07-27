;;; module-base.el --- Basics -*- lexical-binding: t; -*-

;;; Commentary:

;; Базовые настройки, не касающиеся какого-либо конкретного аспекта работы.

;; https://github.com/emacscollective/no-littering
;; https://www.emacswiki.org/emacs/SaveHist

;;; Code:

(defconst ensure/is64
    (not (null
          (string-match "^x86_64-.*" system-configuration)))
    "Equals t if Emacs works on 64-bit system.")

(defconst ensure/isWindows
    (memq system-type '(cygwin windows-nt ms-dos))
    "Equals t if Emacs works on Windows host system.")

(prefer-coding-system 'utf-8)
(set-language-environment "utf-8")

(fset 'yes-or-no-p 'y-or-n-p)

(setq confirm-kill-emacs 'y-or-n-p)

(setq vc-follow-symlinks t)

;; (global-auto-revert-mode t)


;; Сохранение позиции в посещенных файлах
(use-package saveplace
    :config
    (save-place-mode t)
    :custom
    (save-place-file (expand-file-name
                      (format "%s/var/%s"
                              user-emacs-directory
                              "save-place.el"))))


;; Сохранение истории
(use-package savehist
    :config
    (savehist-mode t)
    :custom
    (history-delete-duplicates t)
    (savehist-file (expand-file-name
                    (format "%s/var/%s"
                            user-emacs-directory
                            "savehist.el"))))


;; Ускорение
(add-hook 'emacs-startup-hook (lambda ()
                                  (setq gc-cons-threshold (* 8 1024 1024))
                                  (setq file-name-handler-alist file-name-handler-alist-original)
                                  (makunbound 'file-name-handler-alist-original)))


;; Настройка работы сборщика мусора
;; (use-package gcmh
;;     :straight t
;;     :config
;;     (gcmh-mode t)
;;     :custom
;;     (gcmh-verbose t)
;;     (gcmh-low-cons-threshold (* 8 1024 1024)))


;; Очистка мусора в файлах
(use-package no-littering
    :straight t
    :custom
    (auto-save-file-name-transforms
     `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

(setq create-lockfiles nil)


;; Управление файлами
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

(use-package dired
    :init
    (setq dired-recursive-deletes 'top))

(add-hook 'before-save-hook
          (lambda ()
              (when buffer-file-name
                  (let ((dir (file-name-directory buffer-file-name)))
                      (when (and (not (file-exists-p dir))
                                 (y-or-n-p (format "Directory %s does not exist. Create it? " dir)))
                          (make-directory dir t))))))


(provide 'module-base)
;;; module-base.el ends here
