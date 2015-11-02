;;; matt-prog-functional.el --- Global settings for functional programming languages.

;;; Copyright (c) 2013-2015 Matthew Chan
;;; Author: Matthew Chan <matt@parametri.city>
;;; URL: http://github.com/themattchan/emacs.d

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
    elm-mode
    scala-mode
    scala2-mode
    erlang-mode))

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
(if *is-mac* (setq coq-prog-name "/opt/local/bin/coqtop"))

(setq coq-default-undo-limit 10000)
(setq proof-general-directory "~/.emacs.d/lisp/ProofGeneral-4.3beta")

(if (file-accessible-directory-p proof-general-directory)
    (load (concat (file-name-as-directory proof-general-directory)
                  "generic/proof-site.el")))

(provide 'matt-prog-functional)
;;; matt-prog-functional.el ends here
