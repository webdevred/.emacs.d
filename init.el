;;; package --- Summary: init file  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; set packages
(setq custom-file (expand-file-name "var/custom.el" user-emacs-directory))

(require 'package)

(defcustom emacsd-prefer-melpa-stable
  nil
  "Prefer stable packages"
  :type 'boolean
  :group 'emacsd)

(setq package-archives
      (if emacsd-prefer-melpa-stable
          '(("gnu" . "https://elpa.gnu.org/packages/")
            ("melpa-stable" . "https://stable.melpa.org/packages/")
            ("melpa" . "https://melpa.org/packages/"))
        '(("gnu" . "https://elpa.gnu.org/packages/")
          ("melpa" . "https://melpa.org/packages/")
          ("melpa-stable" . "https://stable.melpa.org/packages/"))))


(setq use-package-always-ensure t)
(setq use-package-always-defer t)

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package no-littering
  :ensure t
  :demand t)

(use-package diminish)

(use-package magit
  :ensure t
  :hook (magit-status . (lambda () (which-function-mode 0))))

(use-package undo-tree
  :config
  (setq undo-tree-auto-save-history 5)
  :diminish 'undo-tree-mode
  :init
  (global-undo-tree-mode))

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
  :hook (haskell-mode-hook . #'hindent-mode)
  :config
  '((haskell-tags-on-save t)
    (haskell-ormolu-on-save t)))

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
