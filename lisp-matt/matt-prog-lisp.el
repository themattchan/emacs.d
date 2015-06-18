;;==============================================================================
;;                                   (Lisp)
;;==============================================================================
(provide 'matt-prog-lisp)

;; (defvar lisp-power-map (make-keymap))
;; (define-minor-mode lisp-power-mode "Fix keybindings; add power."
;;   :lighter " (power)"
;;   :keymap lisp-power-map
;;   )
;; (define-key lisp-power-map [delete] 'paredit-forward-delete)
;; (define-key lisp-power-map [backspace] 'paredit-backward-delete)

;; (defun matt/engage-lisp-power ()
;;   (lisp-power-mode t))

;; (dolist (mode lisp-modes)
;;   (add-hook (intern (format "%s-hook" mode))
;;             #'matt/engage-lisp-power))

(setq inferior-lisp-program "sbcl")
(setq scheme-program-name "racket")

;;------------------------------------------------------------------------------
;; colourful parens
(require 'rainbow-delimiters)
(defvar my-paren-dual-colors '("deep pink" "royal blue"))
;;'("#a07070" "#7070a0") '("pink" "sky blue")

(cl-loop
 for index from 1 to rainbow-delimiters-max-face-count
 do
 (set-face-foreground
  (intern (format "rainbow-delimiters-depth-%d-face" index))
  (elt my-paren-dual-colors
       (if (cl-evenp index) 0 1))))

(set-face-attribute 'rainbow-delimiters-depth-1-face nil
                    :weight 'bold)

(set-face-attribute 'rainbow-delimiters-unmatched-face nil
                    :foreground 'unspecified
                    :inherit 'error
                    :strike-through t)

(dolist (mode lisp-modes)
  (add-hook (intern (format "%s-hook" mode))
            'rainbow-delimiters-mode))

;; Common Lisp
;; Setup load-path and autoloads
(autoload 'slime-autoloads "slime-autoloads")

(add-hook 'slime-mode-hook
          (progn
            ;; Set lisp system and some contribs
            (setq inferior-lisp-program (shell-command-to-string "which clisp"))
            (setq slime-contribs '(slime-fancy slime-scratch slime-editing-commands))
            ))

;; Scheme

;;(eval-after-load "geiser" (progn '(require xscheme) '(require quack)))

(setq scheme-program-name "mit-scheme")
;;(eval-after-load "geiser" '(require quack))

(autoload 'ac-geiser "ac-geiser")
(add-hook 'geiser-mode-hook 'ac-geiser-setup)
(add-hook 'geiser-repl-mode-hook 'ac-geiser-setup)

(add-to-list 'auto-mode-alist '("\\.rkt\\'" . racket-mode))

;; Clojure
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
