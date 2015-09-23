(provide 'matt-prog-functional)

;; no tabs in functional langs
(defvar functional-langs
  '(tuareg-mode
    haskell-mode
    literate-haskell-mode
    elm-mode
    scala-mode
    scala2-mode))

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
(if *is-mac*
    (setq coq-prog-name "/opt/local/bin/coqtop"))
(load "~/.emacs.d/lisp/ProofGeneral-4.3beta/generic/proof-site.el")
(setq coq-default-undo-limit 10000)
