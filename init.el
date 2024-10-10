;;; package --- Summary: init file  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; set packages
(setq custom-file (expand-file-name "var/custom.el" user-emacs-directory))

(require 'package)

(setq package-archives
        '(("gnu" . "https://elpa.gnu.org/packages/")
          ("melpa" . "https://melpa.org/packages/")
          ("melpa-stable" . "https://stable.melpa.org/packages/")))


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
  :hook (prog-mode . lsp-deferred)
  :custom
  ((lsp-auto-guess-root t)
   (lsp-warn-no-matched-clients nil))
  :commands lsp lsp-deferred)

(defun is-project-using-rye ()
  (let ((venv-dir (concat (projectile-project-root) ".venv"))
        (pyproject (concat (projectile-project-root) "pyproject.toml")))
    (and (projectile-project-p) (eq major-mode 'python-mode) (file-exists-p venv-dir) (file-exists-p pyproject))))

(defun lsp-use-ruff-if-available (orig-fun &rest args)
  "Advice to wrap around `lsp` for handling Python package manager Rye and its language server Ruff."
  (if (is-project-using-rye)
      (let ((lsp-enabled-clients '(list ruff)))
        (apply orig-fun args))
    (apply orig-fun args)))

(advice-add 'lsp :around #'lsp-use-ruff-if-available)
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

(use-package hindent)

(use-package haskell-mode
  :mode (("\\.hs$" . haskell-mode))
  :hook (haskell-mode-hook . #'hindent-mode)
  :config
  '((haskell-tags-on-save t)
    (hindent-reformat-buffer-on-save t)))

(use-package company
  :diminish 'company-mode
  :hook (after-init . global-company-mode))

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

(defun activate-project-venv (orig-fun &rest args)
  "Advice to wrap around `lsp` for activating the venv for a Python project if one exists."
  (let ((old-project (or (treemacs--find-project-for-buffer (buffer-file-name (current-buffer))) 
                         (treemacs-project-at-point))))
    (apply orig-fun args)
    (let* ((new-project (treemacs--find-project-for-buffer (buffer-file-name (current-buffer))))
           (old-project-path (when old-project (treemacs-project->path old-project)))
           (new-project-path (when new-project (treemacs-project->path new-project)))
           (venv-dir (concat (file-name-as-directory new-project-path) ".venv")))
      (unless (string= old-project-path new-project-path)
        (pyvenv-deactivate)
        (when (and (file-exists-p (concat (file-name-as-directory new-project-path) "pyproject.toml"))
                   (file-exists-p venv-dir))
          (pyvenv-activate venv-dir)
          (message "Activated venv: %s" venv-dir))))))

(advice-add 'find-file :around #'activate-project-venv)

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
