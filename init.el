;;; package --- Summary: init file  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; set packages
(setq custom-file (expand-file-name "var/custom.el" user-emacs-directory))

(require 'package)

(setq package-archives
        '(("gnu" . "https://elpa.gnu.org/packages/")
          ("melpa" . "https://melpa.org/packages/")))


(setq use-package-always-ensure t)
(setq use-package-always-defer t)

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package no-littering
  :ensure t
  :demand t)

(use-package diminish
  :init
  (diminish 'auto-revert-mode)
  (diminish 'rainbow-mode)
  (diminish 'eldoc-mode))

(use-package magit
  :ensure t
  :hook (magit-status . (lambda () (which-function-mode 0))))

(use-package undo-tree
  :config
  (setq undo-tree-auto-save-history 5)
  :diminish 'undo-tree-mode
  :init
  (global-undo-tree-mode))

(use-package pyvenv)

(use-package lsp-mode
  :hook ((prog-mode . lsp-deferred)
         (save-buffer . (lambda () (when (lsp-workspaces) (lsp-restart-workspace)))))
  :custom
  ((lsp-auto-guess-root t)
   (lsp-warn-no-matched-clients nil))
  :commands lsp lsp-deferred)

(defun is-project-using-rye ()
  (let ((venv-dir (concat (projectile-project-root) ".venv"))
        (pyproject (concat (projectile-project-root) "pyproject.toml")))
    (and (projectile-project-p) (file-exists-p venv-dir) (file-exists-p pyproject))))

(defun lsp-use-ruff-if-available (orig-fun &rest args)
  "Advice to wrap around `lsp` for handling Python package manager Rye and its language server Ruff."
  (if (and  (eq major-mode 'python-mode) (is-project-using-rye))
      (let ((lsp-enabled-clients '(ruff))
            (lsp-ruff-server-command '("rye" "run" "ruff" "server")))
        (apply orig-fun args))
    (apply orig-fun args)))

(advice-add 'lsp :around #'lsp-use-ruff-if-available)

(defun run-rye-tests ()
  "Run tests and show results in buffer"
  (interactive)
  (if (is-project-using-rye)
      (let* ((output-buffer (get-buffer-create (format "*%s-rye-tests*" (projectile-project-name))))
             (process (start-process-shell-command "rye-tests" output-buffer "rye test -- --color=yes")))
        (with-current-buffer (process-buffer process)
          (setq buffer-read-only t))
        (set-process-filter
         process
         (lambda (proc output)
           (with-current-buffer (process-buffer proc)
             (let ((buffer-read-only nil)
                   (start-point (goto-char (point-max))))
               (insert (replace-regexp-in-string "\r" "\n" output))
               (ansi-color-apply-on-region start-point (point-max))
               (goto-char (point-max))))))
        (switch-to-buffer output-buffer))
    (error "not using rye in project")))

(use-package php-mode)

(use-package web-mode
  :mode
  (("\\.lucius$"  . css-mode)
   ("\\.julius$"  . javascript-mode)
   ("\\.html?$"  . html-mode)))

(use-package hamlet-mode
  :mode
  (("\\.hamlet$"  . hamlet-mode)))

(use-package yaml-mode
  :mode (("\\.ya?ml$" . yaml-mode)))

(use-package haskell-mode
  :mode (("\\.hs$" . haskell-mode))
  :hook ((haskell-mode-hook . #'haskell-collapse-mode)
         (haskell-mode-hook . #'haskell-doc-mode)
         (haskell-mode-hook . #'haskell-indent-mode)
         (haskell-mode-hook . #'interactive-haskell-mode))
  :config
  '((haskell-tags-on-save t)
    (hindent-reformat-buffer-on-save t)
    (haskell-process-show-debug-tips)
    (haskell-doc-prettify-types t)))

(use-package company
  :after lsp-mode
  :diminish 'company-mode
  :hook (prog-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package treemacs
  :init
  (setq treemacs-no-png-images t)
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package projectile
  :diminish 'projectile-mode
  :bind ("C-c C-p" . projectile-command-map)
  :init
  (projectile-mode +1)
  (setq projectile-use-git-grep t))

(use-package which-func
  :init
  (which-function-mode 1)
  :hook (magit-status . (lambda () (which-function-mode 0))))

(use-package rainbow-mode
  :hook prog-mode)

(use-package ido
  :config
  (setq
   ido-enable-flex-matching t
   ido-everywhere t
   ido-ignore-files '("\\`\\.nfs" "\\`#.*" "\\`.*~"))
  :init
  (ido-mode 1))

(defun load-config-file (filename)
  "load file name in this config"
  (let ((filepath (concat (expand-file-name (file-name-directory user-init-file)) filename)))
    (load filepath)))

(load-config-file "misc.el")

(load-theme 'cozy-pink t)
