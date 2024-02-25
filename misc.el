;; -*- lexical-binding: t; -*-
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


;; stuff for writing elisp-koans
(defun get-quoted-form-at-point ()
  "Return the quoted form from the current point."
  (save-excursion
    (let ((code-end (point))
          (code-start nil))
      (while (and (not code-start) (re-search-backward "\n(" nil t))
        (setq code-start (match-end 0)))
      (when code-start
        (read (buffer-substring-no-properties (- code-start 1) code-end))))))

(defun get-test-name (test-def)
    (cadr test-def))

(defun run-elisp-koan-test ()
  "run test"
  (interactive)
  (if (and (bound-and-true-p projectile-mode) (s-ends-with-p "elisp-koans/" (projectile-project-root)))
      (let ((test-def (get-quoted-form-at-point)))
            (load-file (concat (projectile-project-root) "elisp-koans.el"))
            (eval test-def)
            (elisp-koans/run-test (get-test-name test-def)))
    (princ "You are not in the elisp-koans repo")))

;; Major mode hooks
(add-hook 'emacs-lisp-mode-hook
  (lambda ()
    ;; Use spaces, not tabs.
    (progn
      (setq indent-tabs-mode nil)
      (define-key emacs-lisp-mode-map (kbd "C-c C-r") #'run-elisp-koan-test))
    ))

(defun do-calc-clear-calculations ()
  (when (not (equal (calc-stack-size) 0))
    (calc-pop (calc-stack-size))))

(defun calc-clear-calculations ()
  (interactive)
  (when (equal major-mode 'calc-mode)
    (do-calc-clear-calculations)))

(add-hook 'calc-mode-hook
          (lambda () (define-key calc-mode-map (kbd "C-c k") #'calc-clear-calculations)))

(add-hook 'text-mode-hook #'visual-line-mode)

;; set up column numbers
(column-number-mode 1)
(add-hook 'prog-mode-hook (lambda () (display-line-numbers-mode 1)))

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
