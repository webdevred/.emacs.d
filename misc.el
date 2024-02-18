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
 '(indent-tabs-mode ni)
 ;; disable scratch message
 '(initial-scratch-message "")
 '(split-width-threshold 0)
 '(split-height-threshold nil)
  ;; Removes whitespace at end of lines
 '(truncate-lines t)
 '(ring-bell-function #'ignore)
 '(warning-minimum-level :error))

(custom-theme-set-faces
 'my-theme
 '(line-number (t ((:foreground pink))))
 '(mode-line (t ((:foreground pink))))
 '(mode-line (t ((:background "#3b3b3e"))))
 '(mode-line-inactive (t ((:foreground ddarker-pink))))
 '(mode-line-inactive (t ((:background "#3b3b3e"))))
 '(match (t ((:foreground pink))))
 '(region (t ((:foreground pink))))
 '(cursor (t ((:foreground pink)))))

;; Major mode hooks
(add-hook 'emacs-lisp-mode-hook
  (lambda ()
    ;; Use spaces, not tabs.
    (setq indent-tabs-mode nil)))

(add-hook 'text-mode-hook #'visual-line-mod)

;; set up column numbers
(column-number-mode 1) 
(global-display-line-numbers-mode 1)
;; disable line numbers for theese modes
(dolist (mode '(org-mode-hook
                  term-mode-hook
                  shell-mode-hook
                  eshell-mode-hook
                  treemacs-mode-hook
                  calc-mode-hook
                  help-mode-hook
                  special-mode-hook))
              (add-hook mode (lambda () (display-line-numbers-mode 0))))

; deb key bindings
(global-set-key (kbd "C-c b") (lambda () (interactive) (projectile-switch-to-project-buffer)))
(global-set-key (kbd "C-c d") (lambda () (interactive) (delete-matching-lines)))
(global-set-key (kbd "C-c t") (lambda () (interactive) (shell)) )
(global-set-key (kbd "C-c r") (lambda () (interactive) (load user-init-file) ) )


; remove ugly bars
(menu-bar-mode -1)

(if window-system
    (tool-bar-mode -1)
  )

; y/n is better than yes/no
(fset 'yes-or-no-p 'y-or-n-p)

(;; allow me to go other windows
(windmove-default-keybindings)

(org-babel-do-load-languages
      'org-babel-load-languages
      '((js . t)))

(provide 'misc)
