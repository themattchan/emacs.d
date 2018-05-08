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

(defconst matt/lisps
  '(lisp-mode
    emacs-lisp-mode
    common-lisp-mode
    scheme-mode
    racket-mode
    clojure-mode))

(defmacro add-hook-all-lisps (thing-to-hook)
  (let ((all-lisps '(lisp-mode
                     emacs-lisp-mode
                     common-lisp-mode
                     scheme-mode
                     racket-mode
                     clojure-mode)))
    `(progn
     ,@(cl-mapcar
        #'(lambda (some-lisp-mode)
            `(add-hook (intern (format "%s-hook" ',some-lisp-mode)) ,thing-to-hook))
        all-lisps))))

;; (macroexpand '(add-hook-all-lisps 'foo))

(diminish-major-mode 'lisp-mode "(())")
(diminish-major-mode 'scheme-mode "(λ)")
(diminish-major-mode 'clojure-mode "(λclj)")
(diminish-major-mode 'emacs-lisp-mode "(λel)")
(diminish-major-mode 'common-lisp-mode "(λcl)")

(setq inferior-lisp-program "sbcl")
(setq scheme-program-name "racket")
(when *is-mac*
  (setq geiser-racket-binary "/Applications/Racket/bin/racket")
  )

(add-hook 'align-load-hook
          (lambda ()
            (add-to-list 'align-lisp-modes 'racket-mode)))

;;------------------------------------------------------------------------------
;; colourful parens
(use-package rainbow-delimiters
  :diminish

  :init
  (add-hook-all-lisps 'rainbow-delimiters-mode)

  :config

  (cl-loop
   for index from 1 to rainbow-delimiters-max-face-count
   do
   (set-face-attribute
    (intern (format "rainbow-delimiters-depth-%d-face" index))
    nil
    :weight 'bold))

  (set-face-attribute 'rainbow-delimiters-unmatched-face nil
                      :foreground 'unspecified
                      :inherit 'error
                      :strike-through t)
  )

(use-package paredit
  :diminish
  :init
  (add-hook-all-lisps 'paredit-mode))

;; Common Lisp
;; Setup load-path and autoloads
(use-package slime
  :config
  ;; Set lisp system and some contribs
  (setq inferior-lisp-program (shell-command-to-string "which clisp"))
  (setq slime-contribs '(slime-fancy slime-scratch slime-editing-commands)))

(use-package slime-autoloads :after slime)



;; Scheme

;;(eval-after-load "geiser" (progn '(require xscheme) '(require quack)))

;;(setq scheme-program-name "mit-scheme")
;;(eval-after-load "geiser" '(require quack))

;; (use-package ac-geiser)
;; (add-hook 'geiser-mode-hook 'ac-geiser-setup)
;; (add-hook 'geiser-repl-mode-hook 'ac-geiser-setup)

(use-package racket-mode
  :mode "\\.rkt\\'"
  :bind
  (:map racket-mode-map
        ("C-c C-l" . 'racket-run-and-switch-to-repl))
  :config
  (diminish-major-mode 'racket-mode "(λr)")
  (when *is-mac* (setq racket-program "/Applications/Racket/bin/racket")))

;; Clojure
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

(provide 'matt-prog-lisp)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; matt-prog-lisp.el ends here
