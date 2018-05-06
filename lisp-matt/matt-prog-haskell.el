;;; matt-prog-haskell.el --- Haskell mode settings

;;; Copyright (c) 2016-2018 Matthew Chan
;;; Author: Matthew Chan <matt@parametri.city>
;;; URL: http://github.com/themattchan/emacs.d
;;; Package-Requires: ((s "1.12.0") (dash "2.13.0") (dash-functional "1.2.0") (emacs "24"))

;;; License:
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

(defconst haskell-modes-list '(haskell-mode literate-haskell-mode purescript-mode))

(eval-and-compile
  (defun haskell-make-tags ()
    (interactive)
    (let ((default-directory (projectile-project-root)))
      (shell-command "hasktags --ignore-close-implementation --etags .")
      (message "[haskell] generated TAGS file")))

  ;; Typecheck *library* only by compiling with -fno-code
  (defun matt/haskell-cabal-typecheck ()
    (interactive)
    (compile "cabal build -j8 --ghc-options=\"-fno-code -fwrite-interface\""))

  ;; Captures the selected region or symbol at point
  ;; and queries hayoo.
  (defun haskell-hayoo-at-point ()
    (interactive)
    (let ((thing (selected-or-symbol-at-point)))
      (when thing (hayoo-query thing))))

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
            ;; { List[x:String] | x = "{-# LANGUAGE <ext1>, ..., <extn> #-}" }
            (s-lines (buffer-substring-no-properties $from $to)))

           (get-extensions ;; String -> List[String]
            (lambda (s)
              (mapcar 's-trim
                      (s-split ","
                               (s-chop-prefix "{-# LANGUAGE"
                                              (s-chop-suffix "#-}" s))))))

           (formatted-extension-list
            ;; { xs:List[x:String] | sortedBy <ext> xs, x = "{-# LANGUAGE <ext> #-}" }
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

  (defun my-nix-current-sandbox ()
    "Try to find if the current project is a nix project.
Returns the project root with a shell.nix file, or NIL if not nix."
    (locate-dominating-file default-directory "shell.nix"))

  ;; The following three functions are copied from
  ;; https://github.com/jml/emacs-configuration/blob/master/plugins/ghc-nix.el
  ;; with modifications
  (defun -wrap-nix-command (command)
    "Wrap the shell command COMMAND to be invoked inside a nix-shell."
    (combine-and-quote-strings
     `("nix-shell"
       "-I" ,(substitute-in-file-name "nixpkgs=$HOME/.nix-defexpr/channels/nixpkgs/")
       "--show-trace"
       "--run" ,command
       "|" "tail" "-1"))) ;; get rid of info messages we print out

  (defun -run-nix-command (command)
    "Run COMMAND inside the current sandbox (implicitly)."
    (let ((default-directory (my-nix-current-sandbox)))
      (shell-command-to-string (-wrap-nix-command command))))

  (defun nix-ghc-executable ()
    "Return the path to the ghc executable inside the current sandbox."
    (s-trim (-run-nix-command "type -p ghc")))

  ;; ;; If in nix, compile inside a nix shell
  ;; (defadvice compile (around maybe-compile-in-nix-sandbox activate)
  ;;   (let ((sandbox (my-nix-current-sandbox)))
  ;;     (if sandbox
  ;;         ;; override dynamically bound variables used by 'compile'
  ;;         (let ((default-directory (my-nix-current-sandbox))
  ;;               (command
  ;;                (combine-and-quote-strings `("nix-shell" "-I" ,(substitute-in-file-name "nixpkgs=$HOME/.nix-defexpr/channels/nixpkgs/")
  ;;                                             "--run" ,command))))

  ;;                                         ;(nix-shell-string (my-nix-current-sandbox) command))); (s-join " " (haskell-make-bash-nix-ghc-command (list command)))))
  ;;           ad-do-it)
  ;;       ad-do-it)))

  (defun haskell-compile-in-nix (cmd)
    (interactive "Mcompile: ")
    (let ((default-directory (my-nix-current-sandbox)))
      (compile (nix-shell-string (my-nix-current-sandbox) cmd))))

  ) ;; eval-and-compile

(eval-after-load 'company-mode '(add-to-list company-backends 'company-ghc))

;; auto-fill-mode is fucked in literate haskell
(add-hook 'literate-haskell-mode-hook (lambda () (auto-fill-mode nil)))

(use-package haskell-mode
  :diminish "λ"
  :ensure t
  :defer t
  :bind
  (:map haskell-mode-map

        ;; Haskell mode keybindings
        ;;        ("C-`" . flycheck-list-errors)
        ("C-c C-b" . flycheck-buffer)
        ("M-n" . flycheck-next-error)
        ("M-p" . flycheck-previous-error)
        ("<f8>" . haskell-navigate-imports)
        ("C-c C-h" . haskell-hoogle)
        ("C-c C-y" . haskell-hayoo-at-point)
        ("C-c g"   . matt/haskell-cabal-typecheck)
        ("C-c i e" . haskell-insert-language-extension)
        ("C-c i o" . haskell-insert-compiler-extension)
        ("C-c i s" . haskell-format-language-extensions))


  :config
  (remove-hook 'haskell-mode-hook 'stack-mode)
  (add-hook 'haskell-mode-hook 'haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'haskell-decl-scan-mode)
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (setq-local haskell-process-suggest-remove-import-lines t)
  (setq-local haskell-process-auto-import-loaded-modules t)
  (setq-local haskell-process-log t)

  ;; Haskell mode GLOBAL settings
  (use-package flycheck
    :diminish
    :init (flycheck-mode 1))

  (use-package flycheck-haskell
    :config (flycheck-haskell-setup))

  ;; use flycheck-haskell
  (flycheck-haskell-setup)

  (haskell-indentation-mode)

  (flycheck-add-next-checker 'haskell-ghc '(t . haskell-hlint))

  (setq-local haskell-process-type 'cabal-repl)

  (setq-local flycheck-haskell-runghc-command '("runghc" "-i"))

  (setq-local haskell-process-path-ghci "cabal repl")
  ;; ;; so we can actually see our writings
  (setq haskell-literate-comment-face 'default)
  (setq haskell-interactive-popup-error nil)

  ;; idk why this was removed from haskell-mode upstream...
  (dolist (mode haskell-modes-list)
    (font-lock-add-keywords mode '(("\\_<\\(error\\|undefined\\)\\_>" 0 'font-lock-warning-face))))

  ) ;; use-package


;; align rules
(add-hook 'align-load-hook
          (lambda ()
            (add-to-list 'align-rules-list
                         '(haskell-types
                           (regexp . "\\(\\s-+\\)\\(::\\|∷\\)\\s-+")
                           (modes haskell-modes-list)))
            (add-to-list 'align-rules-list
                         '(haskell-assignment
                           (regexp . "\\(\\s-+\\)=\\s-+")
                           (modes haskell-modes-list)))
            (add-to-list 'align-rules-list
                         '(haskell-arrows
                           (regexp . "\\(\\s-+\\)\\(->\\|→\\)\\s-+")
                           (modes haskell-modes-list)))
            (add-to-list 'align-rules-list
                         '(haskell-left-arrows
                           (regexp . "\\(\\s-+\\)\\(<-\\|←\\)\\s-+")
                           (modes haskell-modes-list)))
            ))

;; nix stuff
(use-package nix-mode
  :defer t
  :config
  (setq-local tab-width 2))

(provide 'matt-prog-haskell)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; matt-prog-haskell.el ends here
