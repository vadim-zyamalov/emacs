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

(global-auto-revert-mode t)


;; Сохранение позиции в посещенных файлах
(setup saveplace
    (:option save-place-file (expand-file-name
                              (format "%s/var/%s"
                                      user-emacs-directory
                                      "save-place.el")))
    (save-place-mode t))


;; Сохранение истории
(setup savehist
    (:option history-delete-duplicates t
             savehist-file (expand-file-name
                            (format "%s/var/%s"
                                    user-emacs-directory
                                    "savehist.el")))
    (savehist-mode t))


;; Ускорение
(add-hook 'emacs-startup-hook (lambda ()
                                  (setq gc-cons-threshold (* 8 1024 1024))
                                  (setq file-name-handler-alist file-name-handler-alist-original)
                                  (makunbound 'file-name-handler-alist-original)))


;; Настройка работы сборщика мусора
;; (setup (:straight gcmh)
;;     (:option gcmh-verbose t
;;              gcmh-low-cons-threshold (* 8 1024 1024))
;;     (gcmh-mode t))


;; Очистка мусора в файлах
(setup (:straight no-littering)
    (setq auto-save-file-name-transforms
          `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

(setq create-lockfiles nil)


;; Управление файлами
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

(setup dired
    (:option dired-recursive-deletes 'top))

(add-hook 'before-save-hook
          (lambda ()
              (when buffer-file-name
                  (let ((dir (file-name-directory buffer-file-name)))
                      (when (and (not (file-exists-p dir))
                                 (y-or-n-p (format "Directory %s does not exist. Create it? " dir)))
                          (make-directory dir t))))))


(provide 'module-base)
;;; module-base.el ends here
