;;  -*- lexical-binding: t -*-
;; ----------------------------------------------------------------------
;; Haskell
;; ----------------------------------------------------------------------

(use-package haskell-mode :ensure t)
;; (use-package dante
;;   :ensure t
;;   :commands 'dante-mode
;;   :init
;;   (add-hook 'haskell-mode-hook 'dante-mode)
;;   (add-hook 'haskell-mode-hook 'flycheck-mode))
(defun my-nix-current-sandbox ()
  (locate-dominating-file default-directory "shell.nix"))

(defun haskell-custom-hook ()
  (haskell-indentation-mode)

  ;; (define-key intero-mode-map (kbd "C-`") 'flycheck-list-errors)
  ;; (define-key intero-mode-map [f12] 'intero-devel-reload)

  ;; use flycheck-haskell
  (flycheck-haskell-setup)

  (define-key haskell-mode-map (kbd "C-c C-l") #'haskell-interactive-bring)
  ;; (define-key haskell-mode-map [?\C-c ?\C-z] #'haskell-interactive-switch)
  ;; (define-key haskell-mode-map (kbd "C-c C-i") #'haskell-process-do-info)
  ;; (define-key haskell-mode-map [f8] #'haskell-navigate-imports)
  ;; (define-key haskell-mode-map "C-c C-h" 'haskell-hoogle)
  ;; (define-key haskell-mode-map "C-c C-b" 'flycheck-buffer)
  ;; (define-key haskell-mode-map "M-n" 'flycheck-next-error)
  ;; (define-key haskell-mode-map "M-p" 'flycheck-previous-error)

  ;; (define-key haskell-cabal-mode-map (kbd "C-c C-k") #'haskell-interactive-mode-clear)
  ;; (define-key haskell-cabal-mode-map (kbd "C-c c") #'haskell-process-cabal)

  (cond
   ((locate-dominating-file default-directory "shell.nix")
    (progn
      (message "HASKELL: found shell.nix")
      (flycheck-select-checker 'haskell-ghc)
      (flycheck-add-next-checker 'haskell-ghc '(t . haskell-hlint))
      (custom-set-variables
       '(haskell-process-type 'cabal-repl)
       ;; '(flycheck-haskell-runghc-command
       ;;   `(,(substitute-in-file-name "$HOME/.nix-profile/bin/nix-shell")
       ;;   "--run" "cabal repl"))

       '(flycheck-haskell-runghc-command
         '("runghc" "-i"))

       '(haskell-process-path-ghci "cabal repl")

       '(haskell-process-wrapper-function
         #'(lambda (args)
             ;;(apply 'nix-shell-command (nix-current-sandbox) args)
             (list "bash" "-c" (format "source %s; %s" (nix-sandbox-rc (my-nix-current-sandbox)) (s-join " --run " args)))
             ))

       '(flycheck-command-wrapper-function
         #'(lambda (command)
             ;; don't fuck up other modes
             (if (numberp (position major-mode '(haskell-mode literate-haskell-mode)))
                 ;;                 (apply 'nix-shell-command (nix-current-sandbox) command)
                 (list "bash" "-c" (format "source %s; %s" (nix-sandbox-rc (my-nix-current-sandbox)) (s-join " --run " command)))
               command)))

       '(flycheck-executable-find
         #'(lambda (cmd) (nix-executable-find (my-nix-current-sandbox) cmd)))

       ;; '(haskell-process-wrapper-function
       ;;   #'(lambda (argv)
       ;;        `("nix-shell" "-I"
       ;;         ,(substitute-in-file-name "nixpkgs=$HOME/.nix-defexpr/channels/nixpkgs/")
       ;;         "--command")
       ;;        ,(mapconcat 'identity argv " "))))

       ;; '(haskell-process-path-ghci
       ;;   `(,(substitute-in-file-name "$HOME/.nix-profile/bin/nix-shell") "--run" "cabal repl"))
       )))


   ((executable-find "stack")
    (progn
      (custom-set-variables
       '(haskell-process-type 'stack-ghci)
       '(flycheck-haskell-runghc-command
         '("stack" "--verbosity" "silent" "runghc" "--no-ghc-package-path" "--" "--ghc-arg=-i"))
       '(haskell-process-path-ghci "stack"))

      (flycheck-select-checker 'haskell-stack-ghc)
      (flycheck-add-next-checker 'haskell-stack-ghc '(t . haskell-hlint))
      ))

   (t
    (custom-set-variables
     '(flycheck-haskell-runghc-command
       '("runghc" "-i"))
     '(haskell-process-path-ghci "stack")))
   ) ;; cond

   (custom-set-variables
    ;; '(haskell-process-use-ghci t)
    ;;     '(haskell-process-args-ghci '("nix-shell" "--run" "cabal repl")) ; '("ghci" "--with-ghc" "intero" "--no-load" "--no-build"))
    '(haskell-process-suggest-remove-import-lines t)
    '(haskell-process-auto-import-loaded-modules t)
    '(haskell-process-log t))

   ;; (require 'flycheck-liquidhs)
   ;; (require 'liquid-types)


   ;;    (flycheck-add-next-checker 'haskell-hlint '(t . haskell-liquid))
   ;;    (liquid-types-mode 1)

   ) ;; defun

(defun nix-compile-current-sandbox (command)
  (interactive "Mcommand: ")
  ;; override dynamically bound variable used by 'compile'
  (let ((default-directory (my-nix-current-sandbox)))
    (compile (nix-shell-string default-directory command))))

;;(require 'haskell-interactive-mode)
;;(require 'haskell-process)
;; (require 'intero)

(remove-hook 'haskell-mode-hook 'interactive-haskell-mode)
(remove-hook 'haskell-mode-hook 'stack-mode)
;; (add-hook 'haskell-mode-hook 'haskell-doc-mode)
;; (add-hook 'haskell-mode-hook 'haskell-decl-scan-mode)
;; (add-hook 'haskell-mode-hook 'intero-mode)
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
