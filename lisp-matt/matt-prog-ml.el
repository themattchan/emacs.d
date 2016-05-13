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

(defun haskell-custom-hook ()
    (require 'haskell-interactive-mode)
    (require 'haskell-process)
    (add-hook 'haskell-mode-hook #'interactive-haskell-mode)
    (add-hook 'haskell-mode-hook 'haskell-doc-mode)
    (add-hook 'haskell-mode-hook 'haskell-decl-scan-mode)
    (haskell-indentation-mode)

    ;; Load the current file (and make a session if not already made).
    (define-key haskell-mode-map (kbd "C-c C-l") #'haskell-process-load-file)
    ;; Switch to the REPL.
    (define-key haskell-mode-map [?\C-c ?\C-z] #'haskell-interactive-switch)
    ;; “Bring” the REPL, hiding all other windows apart from the source
    ;; and the REPL.
    (define-key haskell-mode-map (kbd "C-`") #'haskell-interactive-bring)
    ;; Get the type and info of the symbol at point, print it in the
    ;; message buffer.
    (define-key haskell-mode-map (kbd "C-c C-t") #'haskell-process-do-type)
    (define-key haskell-mode-map (kbd "C-c C-i") #'haskell-process-do-info)
    ;; Jump to the imports. Keep tapping to jump between import
    ;; groups. C-u f8 to jump back again.
    (define-key haskell-mode-map [f8] #'haskell-navigate-imports)

    (define-key haskell-cabal-mode-map (kbd "C-`") #'haskell-interactive-bring)
    (define-key haskell-cabal-mode-map (kbd "C-c C-k") #'haskell-interactive-mode-clear)
    (define-key haskell-cabal-mode-map (kbd "C-c C-c") #'haskell-process-cabal-build)
    ;; ;; Interactively choose the Cabal command to run.
    (define-key haskell-cabal-mode-map (kbd "C-c c") #'haskell-process-cabal)

    (custom-set-variables
     '(haskell-process-type 'stack-ghci)
     '(haskell-process-suggest-remove-import-lines t)
     '(haskell-process-auto-import-loaded-modules t)
     '(haskell-process-log t))

)

 (use-package haskell-mode
    :ensure t
    :config
    (progn
      (bind-key "C-c C-c" #'haskell-compile haskell-mode-map)
      ;; (add-hook 'haskell-mode-hook (lambda () (ghc-init)))
      ))

(add-hook 'haskell-mode-hook #'haskell-custom-hook)

;; liquidhaskell
;;(require 'flycheck-sandbox-hdevtools)
;;(require 'flycheck-liquidhs)
;;(require 'liquid-types)

;;(eval-after-load 'flycheck '(require 'liquid-hdevtools))
;;(eval-after-load 'flycheck '(require 'hdevtools))

;; (add-hook 'haskell-mode-hook
;;    (lambda () (define-key haskell-mode-map (kbd "C-c .")
;;             'hdevtools/show-type-info)))

;;(add-hook 'haskell-mode-hook (local-set-key (kbd "RET") 'newline-and-indent-relative))

;; ghc-mod
;; (autoload 'ghc-init "ghc" nil t)
;; (autoload 'ghc-debug "ghc" nil t)


;; (require 'haskell-interactive-mode)
;; (require 'haskell-process)
;; ;;(require 'stack-mode)

;; (setq haskell-process-type 'stack-ghci)
;; (setq haskell-process-suggest-remove-import-lines t)
;; (setq haskell-process-auto-import-loaded-modules t)
;; (setq haskell-process-log t)
;; ;; (setq haskell-process-path-ghci "stack")
;; ;; (setq haskell-process-args-ghci "ghci")
;; ;; (setq haskell-program-name "stack ghci")

;; ;(add-hook 'haskell-mode-hook 'stack-mode)
;; (add-hook 'haskell-mode-hook 'haskell-doc-mode)
;; (add-hook 'haskell-mode-hook 'haskell-indentation-mode)
;; (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
;; (add-hook 'haskell-mode-hook 'haskell-decl-scan-mode)
;; ;(add-hook 'haskell-interactive-mode-hook 'structured-haskell-repl-mode)

;; ;;(add-hook 'haskell-mode-hook 'ghc-init)
;; (setq-default flycheck-disabled-checkers  '(haskell-ghc))

;; (defun matt/haskell-keybinds ()
;;   (bind-keys :map haskell-mode-map ("C-`"          . 'haskell-interactive-bring      ))
;;   (bind-keys :map haskell-mode-map ("C-,"          . 'haskell-move-nested-left       ))
;;   (bind-keys :map haskell-mode-map ("C-."          . 'haskell-move-nested-right      ))
;;   (bind-keys :map haskell-mode-map ("C-c C-c"      . 'haskell-compile                ))
;;   (bind-keys :map haskell-mode-map ("C-c C-l"      . 'haskell-process-load-or-reload ))
;;   (bind-keys :map haskell-mode-map ("C-c C-z"      . 'haskell-interactive-switch     ))
;;   (bind-keys :map haskell-mode-map ("C-c C-k"      . 'haskell-interactive-mode-clear ))
;;   (bind-keys :map haskell-mode-map ("C-c C-n C-t"  . 'haskell-process-do-type        ))
;;   (bind-keys :map haskell-mode-map ("C-c C-n C-i"  . 'haskell-process-do-info        ))
;;   (bind-keys :map haskell-mode-map ("C-c C-n C-c"  . 'haskell-process-cabal-build    ))
;;   (bind-keys :map haskell-mode-map ("C-c C-n c"    . 'haskell-process-cabal          ))
;;   (bind-keys :map haskell-mode-map ([tab]          . 'indent-for-tab-command         ))
;;   (bind-keys :map haskell-cabal-mode-map ("C-c C-c" . 'haskell-compile))
;;   )

;; (defun matt/haskell-hooks ()
;;   (haskell-indentation-mode)
;; ;  (interactive-haskell-mode 1)          ; cabal repl
;;   (setq indent-tabs-mode nil)
;;   ;;  (flycheck-select-checker 'haskell-liquid)
;;   (flycheck-select-checker 'haskell-stack-ghc)
;;   ;;  (liquid-types-mode 1)
;;   (electric-indent-mode 0)

;;   ;; Load the current file (and make a session if not already made).
;;   (define-key haskell-mode-map (kbd "C-c C-l") #'haskell-process-load-or-reload)
;;   ;; Switch to the REPL.
;;   (define-key haskell-mode-map [?\C-c ?\C-z] #'haskell-interactive-switch)
;;   ;; “Bring” the REPL, hiding all other windows apart from the source
;;   ;; and the REPL.
;;   (define-key haskell-mode-map (kbd "C-`") #'haskell-interactive-bring)
;;   ;; Get the type and info of the symbol at point, print it in the
;;   ;; message buffer.
;;   (define-key haskell-mode-map (kbd "C-c C-t") #'haskell-process-do-type)
;;   (define-key haskell-mode-map (kbd "C-c C-i") #'haskell-process-do-info)

;;   ;; Jump to the imports. Keep tapping to jump between import
;;   ;; groups. C-u f8 to jump back again.
;;   (define-key haskell-mode-map [f8] #'haskell-navigate-imports)

;;   (define-key haskell-cabal-mode-map (kbd "C-`") #'haskell-interactive-bring)
;;   (define-key haskell-cabal-mode-map (kbd "C-c C-k") #'haskell-interactive-mode-clear)
;;   (define-key haskell-cabal-mode-map (kbd "C-c C-c") #'haskell-process-cabal-build)
;;   ;; ;; Interactively choose the Cabal command to run.
;;   (define-key haskell-cabal-mode-map (kbd "C-c c") #'haskell-process-cabal)
;;   )


;; ;; (eval-after-load 'flycheck
;; ;;   '(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))

;; ;; (add-hook 'flycheck-mode-hook
;; ;;       (lambda () (require 'flycheck-liquidhs)
;; ;;         (flycheck-add-next-checker 'haskell-stack-ghc 'haskell-hlint)
;; ;;         (flycheck-add-next-checker 'haskell-hlint 'haskell-liquid)))

;; ;; (add-hook 'flycheck-after-syntax-check-hook
;; ;;           (lambda () (liquid-tip-update 'flycheck)))

;; (add-hook 'haskell-mode-hook 'matt/haskell-hooks)
;; (add-hook 'literate-haskell-mode-hook 'matt/haskell-hooks)

;; ;; so we can actually see our writings
(setq haskell-literate-comment-face 'default)
(setq haskell-interactive-popup-error nil)


(eval-after-load 'company-mode '(add-to-list 'company-backends 'company-ghc))


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
