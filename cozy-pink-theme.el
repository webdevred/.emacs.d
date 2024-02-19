(deftheme cozy-pink
  "A simple theme for Emacs.")

(setq-default nlinum-format " %d ")

(let ((class '((class color) (min-colors 256)))
      (bg1 "#000000")
      (bg2 "#666666")
      (fg1 "#ffaaff")
      (fg2 "#ff55ff")
      )
  (custom-theme-set-faces
   'cozy-pink

   `(default ((,class (:foreground ,fg1 :background ,bg1))))

   `(mode-line ((,class (:foreground ,fg1 :background ,bg2))))
   `(mode-line-inactive ((,class (:foreground ,fg1 :background ,bg1))))

   `(line-number ((,class (:foreground ,fg1))))
   `(match ((,class (:background ,bg2))))
   `(isearch ((,class (:foreground ,fg2 :background ,bg2))))

   `(cursor ((,class (:foreground ,fg1 :background ,bg2))))
   `(region ((,class (:background ,bg2))))
   ))

(provide-theme 'cozy-pink)
(provide 'cozy-pink-theme)
