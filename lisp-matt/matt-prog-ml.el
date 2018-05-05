;;; matt-prog-ml.el --- Settings for the ML-like languages.

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

;; Including Haskell, SML, OCaml

;;; Code:

;; ----------------------------------------------------------------------
;; OCaml
;; ----------------------------------------------------------------------

(add-hook 'tuareg-mode-hook 'tuareg-imenu-set-imenu)
(setq auto-mode-alist
      (append '(("\\.ml[ily]?$" . tuareg-mode)
                ("\\.topml$" . tuareg-mode))
              auto-mode-alist))
(add-to-list 'auto-mode-alist '("\\.mll\\'" . sml-lex-mode))
(add-to-list 'auto-mode-alist '("\\.mly\\'" . sml-yacc-mode))

;(autoload 'utop-setup-ocaml-buffer "utop" "Toplevel for OCaml" t)
;(add-hook 'tuareg-mode-hook 'utop-setup-ocaml-buffer)
(setq merlin-error-after-save nil)


(autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
(autoload 'camldebug "camldebug" "Run the Caml debugger" t)
(add-hook 'tuareg-mode-hook (lambda ()  (setq indent-tabs-mode nil)))


;; Add opam emacs directory to the load-path
;(setq opam-share (substring (shell-command-to-string "opam config var share 2> /dev/null") 0 -1))
;(add-to-list 'load-path (concat (file-name-directory opam-share) "/emacs/site-lisp"))
;; Load merlin-mode
(use-package merlin)
;; Start merlin on ocaml files
(add-hook 'tuareg-mode-hook 'merlin-mode t)
(add-hook 'caml-mode-hook 'merlin-mode t)
;; Enable auto-complete
(setq merlin-use-auto-complete-mode 'easy)
;; Use opam switch to lookup ocamlmerlin binary
(setq merlin-command 'opam)


;; (with-eval-after-load 'merlin
;;   ;; Disable Merlin's own error checking
;;   (setq merlin-error-after-save nil)
;;   ;; Enable Flycheck checker
;;   (flycheck-ocaml-setup))

;; ----------------------------------------------------------------------
;; F#
;; ----------------------------------------------------------------------

(setq inferior-fsharp-program "/usr/local/bin/fsharpi --readline-")
(setq fsharp-compiler "/usr/local/bin/fsharpc")


(provide 'matt-prog-ml)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; matt-prog-ml.el ends here
