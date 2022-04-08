(defmacro load-user-file (file &optional path)
    "Load a FILE from optional PATH during startup."
    (declare (indent 2))
    (let ((path (if path
                        path
                    "")))
        `(load ,(expand-file-name
                 (format "%s/%s/%s"
                         (file-name-directory user-init-file)
                         path
                         file)))))

(load-user-file
        "module-setup.el"
        "modules")
(load-user-file
        "module-base.el"
        "modules")
(load-user-file
        "module-ui.el"
        "modules")
(load-user-file
        "module-help.el"
        "modules")
(load-user-file
        "module-themes.el"
        "modules")
(load-user-file
        "module-input.el"
        "modules")
(load-user-file
        "module-completion.el"
        "modules")
(load-user-file
        "module-edit.el"
        "modules")
(load-user-file
        "module-programming.el"
        "modules")
