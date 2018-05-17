;;; matt-prog-cc.el ---  C family common customizations.

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

;; cc-mode hooks in order:
;; 1. c-initialization-hook, init cc mode once per session (i.e. emacs startup)
;; 2. c-mode-common-hook, run immediately before loading language hook
;; 3. then language hooks:
;;    c, c++, objc, java, idl, pike, awk
;;
;; View syntactic context (to find the setting to set for c-set-offset): C-c C-s

;;; Code:

(eval-and-compile
  (defun matt/c-indent (width tabs)
    (setq-local tab-width width)
    (setq-local indent-tabs-mode tabs)

    (setq-local c-backspace-function 'backward-delete-char)

    (setq-local c-basic-offset width)
    (setq-local c-indent-level width)

    (setq-local c-indent-tabs-mode tabs)
    (setq-local c-tab-always-indent tabs)
    ))

(use-package cc-mode
  :defer t
  :init
  ;;------------------------------------------------------------------------------
  ;; c-initialization-hook, run once on cc-mode init
  ;; (add-hook 'c-initialization-hook )

  ;;------------------------------------------------------------------------------
  ;; c-mode-common-hook, run before lang hooks
  (add-hook 'c-mode-common-hook
            (lambda ()
              ;; (add-to-list 'ac-sources 'ac-source-c-headers)
              ;; (add-to-list 'ac-sources 'ac-source-c-header-symbols t)

              ;;            (matt/c-indent)

              (add-to-list 'c-cleanup-list 'comment-close-slash)

              (setq c-default-style '((c-mode    . "linux")
                                      (c++-mode  . "stroustrup")
                                      (java-mode . "java")
                                      (awk-mode  . "awk")
                                      (other     . "free-group-style")))

              ;; (setq show-paren-style 'expression) ; highlight blocks

              ;; use /law
              (c-toggle-electric-state 1)
              ;;(c-toggle-auto-newline 0)

              ;; Do real-time syntax highlighting, not the delayed stuff.
              (setq font-lock-support-mode 'jit-lock-mode)
              ;;(setq lazy-lock-defer-contextually t)
              ;;(setq lazy-lock-defer-time 0)

              ;; Left-align all pound defines
              (setq c-electric-pound-behavior '(alignleft))

              ;; Don't force indentation of labels to be 1.
              (setq c-label-minimum-indentation 0)))


  ;;------------------------------------------------------------------------------
  ;; C

  (add-hook 'c-mode-hook (lambda () (matt/c-indent 8 t)))

  ;;------------------------------------------------------------------------------
  ;; C++
  (add-hook 'c++-mode-hook
            (lambda ()
              (matt/c-indent 2 nil)
              (c-set-offset 'statement-case-open 0)
              ;; don't indent curly for if...
              (c-set-offset 'substatement-open 0)
              ;; don't indent curly for inline method def
              (c-set-offset 'inline-open 0)

              (setq-default flycheck-gcc-language-standard "c++17"
                            flycheck-clang-language-standard "c++17")
              ))

  ;;------------------------------------------------------------------------------
  ;; Java
                                        ;(autoload 'jtags-mode "jtags" "Toggle jtags mode." t)
                                        ;(require 'eclim)
                                        ;(require 'eclimd)


  ;; (defvar eclimd-port nil
  ;;   "The active eclimd port number")

  ;; (custom-set-variables
  ;;  '(eclim-eclipse-dirs '("/Applications/eclipse"))
  ;;  '(eclim-executable "/Applications/eclipse/eclim"))

  ;; add the emacs-eclim source
  ;; (require 'ac-emacs-eclim-source)
  ;; (ac-emacs-eclim-config)

  (defun my-java-hook ()
    (matt/c-indent 4 nil)
    (c-set-offset 'substatement-open 0)
    ;; Treat Java 1.5 @-style annotations as comments.
    (setq-local c-comment-start-regexp "(@|/(/|[*][*]?))")
    (setq-local fill-column 100)

    (modify-syntax-entry ?@ "< b" java-mode-syntax-table)

    (jtags-mode)
                                        ; (eclim-mode)
                                        ;  (setq eclim-auto-save t)

    ;; Don't newline open curly
    (setq-local c-hanging-braces-alist
          (append '((defun-open after)
                    (defun-close before after)
                    (class-open after)
                    (class-close before after)
                    (namespace-open after)
                    (inline-open after)
                    (inline-close before after)
                    (block-open after)
                    (block-close . c-snug-do-while)
                    (extern-lang-open after)
                    (extern-lang-close after)
                    (statement-case-open after)
                    (substatement-open after))
                  'c-hanging-braces-alist))
    ) ;; my-java-hook


  (add-hook 'java-mode-hook 'my-java-hook)
  );; use-package cc-mode

;;------------------------------------------------------------------------------
;; Assembly
(use-package asm-mode
  :defer t
  :config
  (auto-complete-mode 0)
  ;; for SPARC asm
  (when (string-equal "s" (file-name-extension (buffer-name)))
    (setq-local asm-comment-char ?\!))
  (setq-local tab-width 8)
  (setq-local tab-stop-list (number-sequence 8 120 8))
  (setq-local indent-tabs-mode t))


;;------------------------------------------------------------------------------
;; Protobuf

(use-package protobuf-mode
  :defer t
  :mode "\\.proto$"
  :config
  (matt/c-indent 2 nil))

(provide 'matt-prog-cc)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; matt-prog-cc.el ends here
