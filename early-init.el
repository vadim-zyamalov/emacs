(setq gc-cons-threshold (* 100 1024 1024)
      gc-cons-percentage 0.6)

(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

(setq user-emacs-directory
      (expand-file-name "emacs/" (or (getenv "XDG_CACHE_HOME") "~/.cache/")))

(setq custom-file
      (expand-file-name "custom.el" user-emacs-directory))

(setq savehist-file (expand-file-name (format "%s/var/%s"
                                              user-emacs-directory
                                              "savehist.el")))

(setq save-place-file (expand-file-name (format "%s/var/%s"
                                                user-emacs-directory
                                                "save-place.el")))

(setq package-enable-at-startup nil
      package-quickstart nil)

(setq straight-check-for-modifications '(check-on-save find-when-checking))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
        (with-current-buffer
                (url-retrieve-synchronously
                 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
                 'silent 'inhibit-cookies)
            (goto-char (point-max))
            (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))
