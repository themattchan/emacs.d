;;; matt-prog-lisp.el --- Settings for the Lisp family.

;;; Copyright (c) 2013-2015 Matthew Chan
;;; Author: Matthew Chan <matt@parametri.city>
;;; URL: http://github.com/themattchan/emacs.d

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Including Common Lisp, MIT Scheme, Racket, Clojure

;;; Code:

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
(when *is-mac*
  (setq geiser-racket-binary "/Applications/Racket/bin/racket")
  (setq racket-program "/Applications/Racket/bin/racket")
  )

(add-hook 'align-load-hook
          (lambda ()
            (add-to-list 'align-lisp-modes 'racket-mode)))

(add-hook 'racket-mode-hook
          #'(lambda ()
              (define-key racket-mode-map (kbd "C-c C-l") 'racket-run-and-switch-to-repl)))

;;------------------------------------------------------------------------------
;; colourful parens
(use-package rainbow-delimiters)
;; (defvar my-paren-dual-colors '("deep pink" "royal blue"))
;; ;;'("#a07070" "#7070a0") '("pink" "sky blue")

(cl-loop
 for index from 1 to rainbow-delimiters-max-face-count
 do
 (set-face-attribute
  (intern (format "rainbow-delimiters-depth-%d-face" index))
  nil
  :weight 'bold))
  ;(elt my-paren-dual-colors
   ;    (if (cl-evenp index) 0 1))))


(set-face-attribute 'rainbow-delimiters-unmatched-face nil
                    :foreground 'unspecified
                    :inherit 'error
                    :strike-through t)

(dolist (mode lisp-modes)
  (add-hook (intern (format "%s-hook" mode))
            'rainbow-delimiters-mode))

;; Common Lisp
;; Setup load-path and autoloads
(use-package slime-autoloads)

(add-hook 'slime-mode-hook
          (progn
            ;; Set lisp system and some contribs
            (setq inferior-lisp-program (shell-command-to-string "which clisp"))
            (setq slime-contribs '(slime-fancy slime-scratch slime-editing-commands))
            ))

;; Scheme

;;(eval-after-load "geiser" (progn '(require xscheme) '(require quack)))

;;(setq scheme-program-name "mit-scheme")
;;(eval-after-load "geiser" '(require quack))

;; (use-package ac-geiser)
;; (add-hook 'geiser-mode-hook 'ac-geiser-setup)
;; (add-hook 'geiser-repl-mode-hook 'ac-geiser-setup)

(add-to-list 'auto-mode-alist '("\\.rkt\\'" . racket-mode))

;; Clojure
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

(provide 'matt-prog-lisp)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; matt-prog-lisp.el ends here
