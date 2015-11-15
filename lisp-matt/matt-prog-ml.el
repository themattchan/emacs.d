;;; matt-prog-ml.el --- Settings for the ML family.

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
;; Haskell
;; ----------------------------------------------------------------------

;; liquidhaskell
;;(require 'flycheck-sandbox-hdevtools)
;;(require 'flycheck-liquid)
;;(require 'liquid-tip)

;;(eval-after-load 'flycheck '(require 'liquid-hdevtools))
;;(eval-after-load 'flycheck '(require 'hdevtools))

;; (add-hook 'haskell-mode-hook
;;    (lambda () (define-key haskell-mode-map (kbd "C-c .")
;;             'hdevtools/show-type-info)))

;;(add-hook 'haskell-mode-hook (local-set-key (kbd "RET") 'newline-and-indent-relative))

;; ghc-mod
(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))
(eval-after-load 'company-mode '(add-to-list 'company-backends 'company-ghc))

(defun matt/haskell-hooks ()
  (haskell-indentation-mode 1)
  (interactive-haskell-mode 1)          ; cabal repl
  (inf-haskell-mode 1)                  ; repl
  (setq indent-tabs-mode nil)
;;  (flycheck-select-checker 'haskell-liquid)
  (liquid-tip-mode 1)
  (electric-indent-mode 0)
  )

(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))

;; (add-hook 'flycheck-after-syntax-check-hook
;;           (lambda () (liquid-tip-update 'flycheck)))

;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'matt/haskell-hooks)
(add-hook 'literate-haskell-mode-hook 'matt/haskell-hooks)

;; so we can actually see our writings
(setq haskell-literate-comment-face 'default)

(defun newline-and-indent-relative ()
  (interactive)
  (newline)
  (indent-to-column (save-excursion
                      (forward-line -1)
                      (back-to-indentation)
                      (current-column))))

(custom-set-variables
 '(haskell-process-type 'stack-ghci)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t))

(eval-after-load 'haskell-mode
  '(progn
     (bind-key "C-`"         'haskell-interactive-bring      haskell-mode-map)
     (bind-key "C-,"         'haskell-move-nested-left       haskell-mode-map)
     (bind-key "C-."         'haskell-move-nested-right      haskell-mode-map)
     (bind-key "C-c C-c"     'haskell-compile                haskell-mode-map)
     (bind-key "C-c C-l"     'haskell-process-load-or-reload haskell-mode-map)
     (bind-key "C-c C-z"     'haskell-interactive-switch     haskell-mode-map)
     (bind-key "C-c C-k"     'haskell-interactive-mode-clear haskell-mode-map)
     (bind-key "C-c C-n C-t" 'haskell-process-do-type        haskell-mode-map)
     (bind-key "C-c C-n C-i" 'haskell-process-do-info        haskell-mode-map)
     (bind-key "C-c C-n C-c" 'haskell-process-cabal-build    haskell-mode-map)
     (bind-key "C-c C-n c"   'haskell-process-cabal          haskell-mode-map)
     (bind-key [tab]         'indent-for-tab-command         haskell-mode-map)
     ;; (bind-key "SPC"      'haskell-mode-contextual-space  haskell-mode-map)
     ;; (bind-key "C-x C-s"  'haskell-mode-save-buffer       haskell-mode-map)
     ))

(eval-after-load 'haskell-cabal
  '(progn
     (bind-key "C-c C-c" 'haskell-compile haskell-cabal-mode-map)))

(setq haskell-interactive-popup-error nil)

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
(setq opam-share (substring (shell-command-to-string "opam config var share 2> /dev/null") 0 -1))
(add-to-list 'load-path (concat (file-name-directory opam-share) "/emacs/site-lisp"))
;; Load merlin-mode
(require 'merlin)
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

(provide 'matt-prog-ml)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; matt-prog-ml.el ends here
