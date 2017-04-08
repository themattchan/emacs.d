;; ----------------------------------------------------------------------
;; Haskell
;; ----------------------------------------------------------------------

(use-package haskell-mode :ensure t)

(defun haskell-custom-hook ()
    (haskell-indentation-mode)

    (define-key intero-mode-map (kbd "C-`") 'flycheck-list-errors)
    (define-key intero-mode-map [f12] 'intero-devel-reload)

    (define-key haskell-mode-map (kbd "C-c C-l") #'intero-repl-load)
    (define-key haskell-mode-map [?\C-c ?\C-z] #'haskell-interactive-switch)
    (define-key haskell-mode-map (kbd "C-c C-i") #'haskell-process-do-info)
    (define-key haskell-mode-map [f8] #'haskell-navigate-imports)
    (define-key haskell-mode-map "C-c C-h" 'haskell-hoogle)
    (define-key haskell-mode-map "C-c C-b" 'flycheck-buffer)
    (define-key haskell-mode-map "M-n" 'flycheck-next-error)
    (define-key haskell-mode-map "M-p" 'flycheck-previous-error)

    ;; (define-key haskell-cabal-mode-map (kbd "C-c C-k") #'haskell-interactive-mode-clear)
    ;; (define-key haskell-cabal-mode-map (kbd "C-c c") #'haskell-process-cabal)

    (custom-set-variables
     ;'(haskell-process-type 'stack-ghci)
     '(haskell-process-type 'ghci)
     '(haskell-process-path-ghci "stack")
     '(haskell-process-use-ghci t)
     '(haskell-process-args-ghci '("ghci" "--with-ghc" "intero" "--no-load" "--no-build"))
     '(haskell-process-suggest-remove-import-lines t)
     '(haskell-process-auto-import-loaded-modules t)
     '(haskell-process-log t))

    (require 'flycheck-liquidhs)
    (require 'liquid-types)

    (flycheck-select-checker 'haskell-stack-ghc)
    (flycheck-add-next-checker 'haskell-stack-ghc '(t . haskell-hlint))
    (flycheck-add-next-checker 'haskell-hlint '(t . haskell-liquid))
    (liquid-types-mode 1)
)

(require 'haskell-interactive-mode)
(require 'haskell-process)
(require 'intero)

(remove-hook 'haskell-mode-hook 'interactive-haskell-mode)
(remove-hook 'haskell-mode-hook 'stack-mode)
(add-hook 'haskell-mode-hook 'haskell-doc-mode)
(add-hook 'haskell-mode-hook 'haskell-decl-scan-mode)
(add-hook 'haskell-mode-hook 'intero-mode)

(add-hook 'haskell-mode-hook #'haskell-custom-hook)

;; ;; so we can actually see our writings
(setq haskell-literate-comment-face 'default)
(setq haskell-interactive-popup-error nil)

;; idk why this was removed from haskell-mode upstream...

(dolist (mode '(haskell-mode literate-haskell-mode))
  (font-lock-add-keywords mode '(("\\_<\\(error\\|undefined\\)\\_>" 0 'font-lock-warning-face))))

(eval-after-load 'company-mode '(add-to-list 'company-backends 'company-ghc))


(provide 'matt-prog-haskell)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; matt-prog-haskell.el ends here
