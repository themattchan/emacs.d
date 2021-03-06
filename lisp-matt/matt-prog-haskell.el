;;; -*- lexical-binding: t -*-
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

  ;; Captures the selected region or symbol atnnn point
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

  (defun haskell-use-nix-p ()
    (locate-dominating-file default-directory "shell.nix"))

  (defun haskell-mode-p ()
    (memq major-mode haskell-modes-list))

  (defun wrap-nix-shell (command)
    `("nix-shell" "--command" ,(string-join command " ")))

  ;; (defun nixify-flycheck-haskell-runghc-command (f &rest args)
  ;;   (let ((res (apply f args))
  ;;         (is-nix (locate-dominating-file default-directory "shell.nix")))
  ;;     (if is-nix
  ;;         (let ((nix-exe (funcall flycheck-executable-find "nix-shell")))
  ;;           `(,nix-exe "-I" "." "--command" (string-join ,res
  ;;       res)
  ;;   )
  ;; (add-function :around (symbol-function 'flycheck-haskell-runghc-command) #'nixify-flycheck-haskell-runghc-command )


  ) ;; eval-and-compile

(eval-after-load 'company-mode '(add-to-list company-backends 'company-ghc))

(use-package lsp-mode
  :commands lsp
  :ensure t
  :config
  (use-package lsp-ui
    :ensure t
    :commands lsp-ui-mode)
  (use-package company-lsp
    :ensure t
    :commands company-lsp)
  (use-package lsp-haskell
    :ensure t
    :init
    (setq lsp-haskell-process-path-hie "hie-wrapper")
    (advice-add 'lsp--haskell-hie-command
                :around
                #'(lambda (fun &rest args)
                    (wrap-nix-shell (apply fun args))))
    (add-hook 'haskell-mode-hook #'lsp)))

(use-package haskell-mode
  :ensure t
  :defer 30
  :mode (("\\.hs\\'"    . haskell-mode)
         ("\\.cabal\\'" . haskell-cabal-mode)
         ("\\.hcr\\'"   . haskell-core-mode)
         ("\\.lhs\\'" . literate-haskell-mode))
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
        ("C-c i s" . haskell-format-language-extensions)
        ("C-c y" . hasky-extensions)
        ("<next> h x" . hasky-extensions-browse-docs)
        )

  :config
  (diminish-major-mode 'haskell-mode "λ")
  (diminish-major-mode 'literate-haskell-mode "λlit")
  (remove-hook 'haskell-mode-hook 'stack-mode)
  ;; (add-hook 'haskell-mode-hook 'haskell-doc-mode)
  ;; (add-hook 'haskell-mode-hook 'haskell-decl-scan-mode)
  ;; (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (setq-local haskell-process-suggest-remove-import-lines t)
  (setq-local haskell-process-auto-import-loaded-modules t)
  ;;  (setq-local haskell-process-log t)

  ;; Haskell mode GLOBAL settings
  (use-package flycheck
    :diminish
    :init (flycheck-mode 1))

  ;; (use-package flycheck-haskell
  ;;   :config (flycheck-haskell-setup))

  ;; (require 'lsp-mode)
  ;; (require 'lsp-haskell)

  (haskell-indentation-mode)

  ;; (flycheck-add-next-checker 'haskell-ghc '(t . haskell-hlint))

  (setq haskell-process-args-cabal-repl '("--ghc-options=-ferror-spans -fshow-loaded-modules -fno-code"))

  ;; (add-hook 'haskell-mode-hook
  ;;           (lambda ()
  ;;             (let ((go (haskell-use-nix-p)))
  ;;               (when go
  ;;                 (flycheck-haskell-setup)
  ;;                 (setq flycheck-haskell-ghc-executable (expand-file-name "nix-ghc" user-emacs-directory))
  ;;                 (flycheck-select-checker 'haskell-ghc)
  ;;                 (setq-local flycheck-ghc-search-path (list (expand-file-name go)))
  ;;                 ;; (setq flycheck-command-wrapper-function
  ;;                 ;;       (lambda (command)
  ;;                 ;;         ;; (message (format "FUCKKKKKKK %s" (list "bash" "-c" (format "(cd %s; nix-shell --command \"%s\")" go (s-join " " command)))))
  ;;                 ;;         ;; (list "bash" "-c" (format "(cd %s; nix-shell --command \"%s\")" go (s-join " " command)))
  ;;                 ;;         ;; (message (format "FUCKKKKKKK %s" (list "bash" "-c" (format "(cd %s; nix-shell --command \"%s\")" go (s-join " " command)))))
  ;;                 ;;         ;; (list "" "-c" (format "(cd %s; nix-shell --command \"%s\")" go (s-join " " command)))
  ;;                 ;;         ;;(message (format "%s" command))
  ;;                 ;;         command
  ;;                 ;;         ))

  ;;                 ;; (setq flycheck-command-wrapper-function
  ;;                 ;;       (lambda (command)
  ;;                 ;;         (message (format "%s" command))
  ;;                 ;;         command))

  ;;                 ))))

;; nix-shell --command "ghcid --command \"cabal repl lib:eaql\""

  ;; ;; so we can actually see our writings
  (setq haskell-literate-comment-face 'default)
  (setq haskell-interactive-popup-error nil)
  ;; auto-fill-mode is fucked in literate haskell
  (add-hook 'literate-haskell-mode-hook
            (lambda ()
              (auto-fill-mode nil)))

  ;; idk why this was removed from haskell-mode upstream...
  (dolist (mode haskell-modes-list)
    (font-lock-add-keywords mode '(("\\_<\\(error\\|undefined\\)\\_>" 0 'font-lock-warning-face))))

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

  ) ;; use-package


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
