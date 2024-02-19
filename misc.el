(custom-set-variables
 ;; set garbaǵe collection high to increase speed
 '(gc-cons-threshold 100000000)
 '(fill-column 80)
 '(fit-window-to-buffer-horizontally t)
 '(menu-bar-select-buffer-function 'switch-to-buffer)
 '(qmouse-avoidance-mode 'banish)
 '(window-resize-pixelwise t)
 ;; disable splash screen
 '(inhibit-splash-screen t)
 '(inhibit-startup-screen t)
 '(inhibit-startup-message t)
 '(indent-tabs-mode nil)
 ;; disable scratch message
 '(initial-scratch-message "")
 '(split-width-threshold 0)
 '(split-height-threshold nil)
  ;; Removes whitespace at end of lines
 '(truncate-lines t)
 '(ring-bell-function #'ignore)
 '(warning-minimum-level :error))

;; Major mode hooks
(add-hook 'emacs-lisp-mode-hook
  (lambda ()
    ;; Use spaces, not tabs.
    (setq indent-tabs-mode nil)))

(add-hook 'text-mode-hook #'visual-line-mod)

;; set up column numbers
(column-number-mode 1)
(global-display-line-numbers-mode 1)

;; i had some problems with treemacs and linenumbers, this solves it
(add-hook 'after-init-hook (lambda ())
          (if (fboundp 'treemacs-get-local-window)
              (let ((treemacs-window (treemacs-get-local-window)))
                (if treemacs-window
                    (with-current-buffer (window-buffer treemacs-window)
                        (display-line-numbers-mode 0))))))

;; disable line numbers for theese modes
(dolist (mode '(completion-list-mode-hook
                calc-mode-hook
                Info-mode-hook
                org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook
                special-mode-hook
                treemacs-mode))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

; deb key bindings
(global-set-key (kbd "C-c b") (lambda () (interactive) (projectile-switch-to-project-buffer)))
(global-set-key (kbd "C-c d") (lambda () (interactive) (delete-matching-lines)))
(global-set-key (kbd "C-c t") (lambda () (interactive) (shell)) )
(global-set-key (kbd "C-c r") (lambda () (interactive) (load user-init-file) ) )


; remove ugly bars
(menu-bar-mode -1)

(if window-system
    (progn (tool-bar-mode -1)
           (scroll-bar-mode -1))
  )

; y/n is better than yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; allow me to go other windows
(windmove-default-keybindings)

(org-babel-do-load-languages
      'org-babel-load-languages
      '((js . t)))

(provide 'misc)
