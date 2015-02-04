;; dired
;; hide the -al stuff, toggle with ( and )
(require 'dired-details+)
(setq dired-recursive-deletes 'always
      dired-recursive-copies 'always
      delete-by-moving-to-trash t
      trash-directory "~/.Trash/"
      ;; show sym link targets
      Dired-details-hide-link-targets nil)
(require 'dired-x)
(setq-default ; M-o to toggle omit mode
 dired-omit-mode t
 dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\.")

;; terminal
;; (use-package multi-term
;;   :ensure t
;;   :commands (multi-term)
;;   :bind (("M-o M-t" . multi-term)
;;          ("M-[" . multi-term-prev)
;;          ("M-]" . multi-term-next))
;;   :init
;;   (progn
;;     (setq multi-term-program "/bin/zsh")))
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; tramp mode
(setq tramp-default-method "scpx")
(eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))

(provide 'matt-utils)
