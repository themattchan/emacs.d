;;; matt-prog-functional.el --- Global settings for functional programming languages.

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

;;; Code:

;; no tabs in functional langs
(defvar functional-langs
  '(tuareg-mode
    sml-lex-mode
    sml-yacc-mode
    sml-mode
    haskell-mode
    literate-haskell-mode
    purescript-mode
    elm-mode
    scala-mode
    scala2-mode
    erlang-mode
    fsharp-mode))

(defvar lisp-modes
  '(lisp-mode
    emacs-lisp-mode
    common-lisp-mode
    scheme-mode
    racket-mode
    clojure-mode))

(defun matt/functional-programming ()
  (setq indent-tabs-mode nil)
  (setq tab-width 2))

(dolist (mode functional-langs)
  (add-hook (intern (format "%s-hook" mode))
            #'matt/functional-programming))

;; Fucking tabs in my Haskell source, be gone!
;; Also, no tabs in FP in general. yuck.
(defun matt/untabify-hook ()
  (when (member major-mode (cl-union functional-langs
                                     lisp-modes))
    (untabify (point-min) (point-max))))
(add-hook 'before-save-hook 'matt/untabify-hook)

;; Proof General and Coq
;; TODO: autoload
(if *is-mac* (setq coq-prog-name "/usr/local/bin/coqtop"))

(setq coq-default-undo-limit 10000)
(setq proof-general-directory "~/.emacs.d/lisp/PG")
(setq company-coq-disabled-features '(prettify-symbols))

(if (file-accessible-directory-p proof-general-directory)
    (load (concat (file-name-as-directory proof-general-directory)
                  "generic/proof-site")))

;;(add-hook 'coq-mode-hook #'company-coq-mode)

;;  Purescript

(add-hook 'purescript-mode-hook
  (lambda ()
    (require 'psc-ide)
    (psc-ide-mode)
    (company-mode)
    (flycheck-mode)
    (purescript-indentation-mode)))

(defun purescript-make-tags ()
  (interactive)
  (projectile-with-default-dir (projectile-project-root)
    (shell-command "purs docs --format etags \"src/**/*.purs\" \"bower_components/*/src/**/*.purs\" >! TAGS")))

(add-hook 'psc-ide-mode-hook
          (lambda ()
            (bind-key "M-." :map psc-ide-mode-map 'xref-find-definitions)))

(setq idris-interpreter-path "/usr/local/bin/idris")

(provide 'matt-prog-functional)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; matt-prog-functional.el ends here
