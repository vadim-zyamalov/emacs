;;; module-programming.el --- Programming -*- lexical-binding: t; -*-

;;; Commentary:

;; Пакеты, настраивающие языки программирования.

;; https://github.com/bbatsov/projectile
;; https://github.com/sabof/org-bullets
;; https://github.org/snosov1/toc-org
;; https://github.com/awth13/org-appear
;; https://ess.r-project.org/

;;; Code:

;; Projectile
(use-package projectile
    :straight t
    :bind-keymap ("C-c p" . projectile-command-map)
    :config
    (projectile-mode t)
    :custom
    (projectile-completion-system 'default))


;; Flycheck
(use-package flycheck
    :straight t
    :config
    (global-flycheck-mode))


;; Markdown
(use-package markdown-mode
    :straight t
    :mode (("README\\.md\\'" . gfm-mode)
           ("\\.md\\'" . markdown-mode)
           ("\\.markdown\\'" . markdown-mode))
    :custom
    (markdown-fontify-code-blocks-natively t)
    (markdown-command "multimarkdown"))


;; Org-mode
(defun my/angle-brackets-fix ()
    (modify-syntax-entry ?< "." org-mode-syntax-table)
    (modify-syntax-entry ?> "." org-mode-syntax-table))

(use-package org
    :straight t
    :hook ((org-mode . org-indent-mode)
           (org-mode . my/angle-brackets-fix))
    :config
    (org-babel-do-load-languages
     'org-babel-load-languages '((emacs-lisp . t)
                                 (python . t)
                                 (lua . t)
                                 (haskell . t)
                                 (shell . t)))
    :config
    (progn
        (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
        (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
        (add-to-list 'org-structure-template-alist '("hs" . "src haskell"))
        (add-to-list 'org-structure-template-alist '("lua" . "src lua"))
        (add-to-list 'org-structure-template-alist '("py" . "src python"))
        (add-to-list 'org-structure-template-alist '("tex" . "src tex")))
    :custom
    (org-edit-src-content-indentation 0)
    (org-src-preserve-indentation nil)
    (org-src-fontify-natively t)
    (org-src-tab-acts-natively t)
    (org-return-follows-link t)
    (org-mouse-1-follows-link t)
    (org-descriptive-links t)
    (org-hide-emphasis-markers t)
    (org-support-shift-select t))

(use-package edit-indirect
    :straight t)

(use-package org-bullets
    :straight t
    :after org
    :hook (org-mode . org-bullets-mode)
    :custom
    (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package toc-org
    :straight t
    :after org
    :hook (org-mode . toc-org-mode))

(use-package org-appear
    :straight (org-appear :type git :host github :repo "awth13/org-appear")
    :after org
    :hook (org-mode . org-appear-mode)
    :custom
    (org-appear-autolinks t)
    (org-appear-autosubmarkers t))


;; Emacs Speaks Statistics --- ESS
(use-package ess
    :straight t
    :mode (("\\.R$" . ess-r-mode)
           ("\\.do$" . ess-stata-mode))
    :hook ((ess-r-mode . lsp/lsp)
           (ess-r-post-run . ess-rdired)
           ((ess-r-mode ess-stata-mode) . (lambda ()
                                              (setq-local fill-column 80)
                                              (display-fill-column-indicator-mode))))
    :init
    (unless (getenv "LC_ALL")
        (setenv "LC_ALL" "ru_RU.UTF-8"))
    (setq display-buffer-alist
          (append `(("^\\*R Dired"
                     (display-buffer-reuse-window display-buffer-in-side-window)
                     (side . right)
                     (slot . -1)
                     (window-width . 0.33)
                     (reusable-frames . nil))
                    ("^\\*R view"
                     (display-buffer-reuse-window display-buffer-in-side-window)
                     (side . right)
                     (slot . 1)
                     (window-width . 0.33)
                     (reusable-frames . nil))
                    ("^\\*R"
                     (display-buffer-reuse-window display-buffer-in-side-window)
                     (side . right)
                     (slot . 1)
                     (window-width . 0.33)
                     (reusable-frames . nil)))
                  display-buffer-alist)))


;; Python
(defun capf/python-mode ()
    "Extra CAPF for `python-mode'."
    (setq completion-at-point-functions
          (append completion-at-point-functions
                  (list 'cape-file))))

(use-package python
    :straight lsp-pyright
    :hook ((python-mode . lsp/lsp)
           (python-mode . (lambda ()
                              (setq-local fill-column 80)
                              (display-fill-column-indicator-mode)))))


;; JS
;; npm i -g typescript-language-server; npm i -g typescript
(use-package js
    :mode "\\.js.R$"
    :hook (js-mode . lsp/lsp))


;; Lua
(use-package lua-mode
    :straight t
    :mode "\\.lua$")


;; LaTeX
(defun capf/latex-mode ()
    "Extra CAPF for `LaTeX-mode'."
    (progn
        (fset 'cape/company-reftex-labels
              (cape-company-to-capf #'company-reftex-labels))
        (fset 'cape/company-reftex-citations
              (cape-company-to-capf #'company-reftex-citations))
        (fset 'cape/company-math-symbols-latex
              (cape-company-to-capf #'company-math-symbols-latex))
        (fset 'cape/company-math-symbols-unicode
              (cape-company-to-capf #'company-math-symbols-unicode))
        (setq completion-at-point-functions
              (append completion-at-point-functions
                      (list 'cape/company-reftex-labels
                            'cape/company-reftex-citations
                            'cape/company-math-symbols-latex
                            'cape/company-math-symbols-unicode)))))

(defun auctex/non-greedy-capf ()
    "Making AUCTeX capf non-greedy."
    (progn
        (fset 'non-greedy-tex
              (cape-capf-properties #'TeX--completion-at-point :exclusive 'no))
        (setq completion-at-point-functions
              (list 'non-greedy-tex))))

(defun auctex/latexmk ()
    "Add a command for TeX-file compilation via latexmk."
    (push
     '("LaTeX Make"
       "latexmk -pdf -cd -f -interaction=nonstopmode -synctex=1 -shell-escape -outdir=output %t"
       TeX-run-TeX nil t
       :help "Make the file using latexmk.")
     TeX-command-list))

(defun my/region-or-env-or-paragraph ()
    "Produce region from LaTeX environment or paragraph if no any already."
    (unless (region-active-p)
        (if (equal major-mode 'latex-mode)
                (LaTeX-mark-environment)
            (mark-paragraph))
        (let ((beg (save-excursion
                       (goto-char (region-beginning))
                       (forward-line)
                       (line-beginning-position)))
              (end (if (equal major-mode 'latex-mode)
                           (save-excursion
                               (goto-char (region-end))
                               (forward-line (if (equal (point) (line-end-position))
                                                     -1
                                                 -2))
                               (line-end-position))
                       (region-end))))
            (set-mark beg)
            (goto-char end))))

(defun my/region-expand-one-char ()
    "Add extra char to the end of region if possible."
    (if (and (= (region-end) (line-end-position))
             (/= (region-end) (line-beginning-position))
             (/= (region-end) (point-max)))
            (1+ (region-end))
        (region-end)))

(defun my/point-add-one-char (end)
    "Add new line if END is the last char and not at line-beginning."
    (interactive "r")
    (save-excursion
        (goto-char end)
        (if (and (equal end (point-max))
                 (equal end (line-end-position))
                 (not (equal end (line-beginning-position))))
                (insert "\n"))))

(defun auctex/table-format (delim)
    "Convert table delimited by DELIM (usually copy-pasted from Excel)
to the LaTeX table."
    (interactive "sEnter delimiter (TAB by default): ")
    (when (string= delim "")
        (setq delim "\t"))
    (save-restriction
        (save-excursion
            (my/region-or-env-or-paragraph)
            (my/point-add-one-char (region-end))
            (narrow-to-region
             (region-beginning)
             (my/region-expand-one-char))
            (goto-char (point-min))
            (while (search-forward-regexp delim nil t)
                (replace-match " & " nil nil))
            (goto-char (point-min))
            (while (search-forward-regexp "\n" nil t)
                (replace-match " \\\\\\\\\n" nil nil)))))


(defun auctex/table-align ()
    "Align LaTeX table by its inner delimeters."
    (interactive)
    (save-restriction
        (save-excursion
            (my/region-or-env-or-paragraph)
            (my/point-add-one-char (region-end))
            (narrow-to-region
             (region-beginning)
             (my/region-expand-one-char))
            (goto-char (point-min))
            (while (search-forward-regexp "[ ]*&[ ]*" nil t)
                (replace-match " & " nil nil)))
        (align-regexp (point-min) (point-max) "\\(\\s-*\\)&"
                      1 1 t)
        (align-regexp (point-min) (point-max) "\\(\\s-*\\)\\\\\\\\"
                      1 1 t)))

(use-package company-reftex
    :straight t)
(use-package company-auctex
    :straight t)
(use-package company-math
    :straight t)

(use-package LaTeX
    :straight auctex
    :hook ((LaTeX-mode . lsp/lsp)
           (LaTeX-mode . auctex/latexmk)
           (LaTeX-mode . turn-on-reftex))
    :custom
    (preview-pdf-color-adjust-method t)
    (preview-auto-cache-preamble t)
    (bibtex-dialect 'biblatex)
    (reftex-cite-format '((?\C-m . "\\cite[]{%l}")
                          (?a . "\\autocite[]{%l}")
                          (?p . "\\parencite[]{%l}")
                          (?f . "\\footcite[][]{%l}")
                          (?t . "\\textcite[]{%l}")
                          (?o . "\\citepr[]{%l}")
                          (?F . "\\fullcite[]{%l}")
                          (?n . "\\nocite{%l}")))
    (reftex-cite-prompt-optional-args t)
    (LaTeX-reftex-cite-format-auto-activate nil)
    (reftex-plug-into-AUCTeX t))


(provide 'module-programming)
;;; module-programming.el ends here
