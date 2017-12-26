;;  -*- lexical-binding: t -*-
;; ----------------------------------------------------------------------
;; Haskell
;; ----------------------------------------------------------------------

(use-package haskell-mode :ensure t)
(require 'ghc-nix)

;; (use-package dante
;;   :ensure t
;;   :commands 'dante-mode
;;   :init
;;   (add-hook 'haskell-mode-hook 'dante-mode)
;;   (add-hook 'haskell-mode-hook 'flycheck-mode))

;; (defun matt/hoogle-search-at-point ()
;;   (interactive "


(defun haskell-insert-language-extension (ext)
  (interactive "MHaskell Extension: ")
  (insert (format "{-# LANGUAGE %s #-}\n" (s-upper-camel-case ext))))

(defun haskell-insert-compiler-extension (ext)
  (interactive "MGHC Extension: ")
  (insert (format "{-# OPTIONS_GHC -f%s #-}\n" (s-dashed-words ext))))

;; http://ergoemacs.org/emacs/elisp_command_working_on_string_or_region.html
(defun haskell-format-language-extensions ($string &optional $from $to)
  (interactive
    (if (use-region-p)
        (list nil (region-beginning) (region-end))
      (let ((bds (bounds-of-thing-at-point 'paragraph)) )
        (list nil (car bds) (cdr bds)))))
  (let* ((input
          ;; { List[x:String] | x = {-# LANGUAGE <ext1>, ..., <extn> #-} }
          (s-lines (buffer-substring-no-properties $from $to)))

         (get-extensions ;; String -> List[String]
          (lambda (s)
           (mapcar 's-trim
            (s-split ","
             (s-chop-prefix "{-# LANGUAGE"
              (s-chop-suffix "#-}" s))))))

         (formatted-extension-list
          ;; { xs:List[x:String] | sortedBy <ext> xs, x = {-# LANGUAGE <ext> #-} }
          (seq-filter (lambda (s) (not (string= "" s)))
           (delete-dups
            (sort-strings
             (apply 'append
              (mapcar get-extensions input))))))
         )
  (save-excursion
    (delete-region $from $to)
    (goto-char $from)
    (mapc 'haskell-insert-language-extension formatted-extension-list)
    )))

(defconst haskell-modes-list '(haskell-mode literate-haskell-mode))

(defun haskell-use-stack-p ()
  (locate-dominating-file default-directory "stack.yaml"))

(defun haskell-mode-p ()
  (memq major-mode haskell-modes-list))

(defun haskell-make-bash-nix-ghc-command (command)
  `("bash" "-c"
    ,(format "source %s;" (nix-sandbox-rc (my-nix-current-sandbox)))
    "-I" ,(substitute-in-file-name "nixpkgs=$HOME/.nix-defexpr/channels/nixpkgs/")
    "--show-trace"
    "--command"
    ,@command
    )
  )

(defun haskell-make-bash-nix-ghci-repl (argv)
  `("bash" "-c"
    ,(format "source %s;" (nix-sandbox-rc (my-nix-current-sandbox)))
    "-I" ,(substitute-in-file-name "nixpkgs=$HOME/.nix-defexpr/channels/nixpkgs/")
    "--show-trace"
    "--command"
    ,(s-join " " argv)
    )
  )

;; If this returns NIL, then not nix
(defun my-nix-current-sandbox ()
  (locate-dominating-file default-directory "shell.nix"))

;; If in nix, compile inside a nix shell
(defadvice compile (around maybe-compile-in-nix-sandbox activate)
  (let ((sandbox (my-nix-current-sandbox)))
    (if sandbox
        ;; override dynamically bound variables used by 'compile'
        (let ((default-directory (my-nix-current-sandbox))
              (command (nix-shell-string default-directory command)))
          ad-do-it)
      ad-do-it)))

;; This is run ONCE when you switch to 'haskell-mode'
(defun haskell-custom-hook ()
  (haskell-indentation-mode)

  ;; (define-key intero-mode-map (kbd "C-`") 'flycheck-list-errors)
  ;; (define-key intero-mode-map [f12] 'intero-devel-reload)
  (use-package flycheck
    :init (flycheck-mode 1))

  (use-package flycheck-haskell
    :config (flycheck-haskell-setup))

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
  (define-key haskell-mode-map (kbd "C-c i e") #'haskell-insert-language-extension)
  (define-key haskell-mode-map (kbd "C-c i o") #'haskell-insert-compiler-extension)
  (define-key haskell-mode-map (kbd "C-c i s") #'haskell-format-language-extensions)

  (cond
   ((my-nix-current-sandbox)
    ;; => USE NIX
    (message "HASKELL: found shell.nix")
    (flycheck-haskell-setup)
    (use-nix-ghc-in-flycheck) ;; see lisp/ghc-nix.el
    (flycheck-select-checker 'haskell-ghc)
    (flycheck-set-checker-executable (nix-ghc-executable))
    (setq flycheck-haskell-ghc-executable (nix-ghc-executable))
    (flycheck-add-next-checker 'haskell-ghc '(t . haskell-hlint))
    (add-to-list 'flycheck-disabled-checkers 'haskell-stack-ghc)

    (setq-local haskell-process-type 'cabal-repl)

    (setq-local flycheck-haskell-runghc-command
                '("runghc" "-i"))

    (setq-local haskell-process-path-ghci "cabal repl")

    ;; C-c C-l repl
    (setq-local haskell-process-wrapper-function
                #'haskell-make-bash-nix-ghci-repl)

    ;; flycheck batch compilation
    (setq-local flycheck-command-wrapper-function
                #'(lambda (command) ;; List[String] -> List[String]
                    (if (and (eq 'haskell-ghc flycheck-checker)
                             (my-nix-current-sandbox)
                             (haskell-mode-p))
                        (progn
                          (message "[flycheck-haskell] Checking buffer")
                          (haskell-make-bash-nix-ghc-command command)
                          )
                      command)
                    ))

    (setq-local flycheck-executable-find
                #'(lambda (cmd) (nix-executable-find (my-nix-current-sandbox) cmd)))

    (message "NIX GHC: %s" (nix-ghc-executable))
    )


   ((and (haskell-use-stack-p) (executable-find "stack"))
    ;; => USE STACK
    (setq-local haskell-process-type 'stack-ghci)
    (setq-local flycheck-haskell-runghc-command
                '("stack" "--verbosity" "silent" "runghc" "--no-ghc-package-path" "--" "--ghc-arg=-i"))
    (setq-local haskell-process-path-ghci "stack")

    (flycheck-select-checker 'haskell-stack-ghc)
    (flycheck-add-next-checker 'haskell-stack-ghc '(t . haskell-hlint))
    (intero-mode)
    )


   (t
    ;; => DEFAULT GHCI
    (flycheck-select-checker 'haskell-ghc)
    (setq-local flycheck-haskell-runghc-command '("runghc" "-i"))
    (setq-local haskell-process-path-ghci "ghci")
    )

   ) ;; cond

  (message "HASKELL: Flycheck checker is %s" flycheck-checker)

  ;;   (custom-set-variables
  ;; '(haskell-process-use-ghci t)
  ;;     '(haskell-process-args-ghci '("nix-shell" "--run" "cabal repl")) ; '("ghci" "--with-ghc" "intero" "--no-load" "--no-build"))
  (setq-local haskell-process-suggest-remove-import-lines t)
  (setq-local haskell-process-auto-import-loaded-modules t)
  (setq-local haskell-process-log t)

  ;; (require 'flycheck-liquidhs)
  ;; (require 'liquid-types)

  ;;    (flycheck-add-next-checker 'haskell-hlint '(t . haskell-liquid))
  ;;    (liquid-types-mode 1)

  ) ;; haskell-custom-hook

;;(require 'haskell-interactive-mode)
;;(require 'haskell-process)
;; (require 'intero)

;;(remove-hook 'haskell-mode-hook 'interactive-haskell-mode)
(remove-hook 'haskell-mode-hook 'stack-mode)
(add-hook 'haskell-mode-hook 'haskell-doc-mode)
(add-hook 'haskell-mode-hook 'haskell-decl-scan-mode)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-mode-hook #'haskell-custom-hook)

;; ;; so we can actually see our writings
(setq haskell-literate-comment-face 'default)
(setq haskell-interactive-popup-error nil)

;; idk why this was removed from haskell-mode upstream...
(dolist (mode matt/haskell-modes-list)
  (font-lock-add-keywords mode '(("\\_<\\(error\\|undefined\\)\\_>" 0 'font-lock-warning-face))))

(eval-after-load 'company-mode '(add-to-list 'company-backends 'company-ghc))

(provide 'matt-prog-haskell)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; matt-prog-haskell.el ends here
