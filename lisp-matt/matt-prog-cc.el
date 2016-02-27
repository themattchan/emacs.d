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

(defun matt/c-indent ()
  ;; use setq-local, its supposedly more hygienic
  ;; set correct backspace behaviour.
  (setq-local c-backspace-function 'backward-delete-char)

  ;; c-type lang specifics. want 4-space width tab tabs
  (setq-local c-basic-offset 4)
  (setq-local tab-width 4)
  ;; (setq-local c-indent-level 4)

  ;;(setq-local c-indent-tabs-mode t)           ; tabs please
  (setq-local c-tab-always-indent t)          ; t for tabs, nil for spaces
  (setq-local indent-tabs-mode t)

  ;;(setq-local tab-stop-list (number-sequence 8 120 8))
  )

;;------------------------------------------------------------------------------
;; c-initialization-hook, run once on cc-mode init
;; (add-hook 'c-initialization-hook )

;;------------------------------------------------------------------------------
;; c-mode-common-hook, run before lang hooks
(add-hook 'c-mode-common-hook
          (lambda ()
            ;; (add-to-list 'ac-sources 'ac-source-c-headers)
            ;; (add-to-list 'ac-sources 'ac-source-c-header-symbols t)

            (matt/c-indent)

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

            ;; subword editing and movement to deal with CamelCase
            (subword-mode 1)

            ;; changed my mind, this is annoying as hell
            (electric-pair-mode 0)

            ;; Do real-time syntax highlighting, not the delayed stuff.
            (setq font-lock-support-mode 'jit-lock-mode)
            ;;(setq lazy-lock-defer-contextually t)
            ;;(setq lazy-lock-defer-time 0)

            ;; Left-align all pound defines
            (setq c-electric-pound-behavior '(alignleft))

            ;; Don't force indentation of labels to be 1.
            (setq c-label-minimum-indentation 0)))

;;------------------------------------------------------------------------------
;; Assembly
(add-hook 'asm-mode-hook
          (lambda ()
            (auto-complete-mode 0)
            ;; for SPARC asm
            (when (string-equal "s" (file-name-extension (buffer-name)))
              (setq-local asm-comment-char ?\!))
            (setq-local tab-width 8)
            (setq-local tab-stop-list (number-sequence 8 120 8))
            (setq-local indent-tabs-mode t)))

;;------------------------------------------------------------------------------
;; C
;(autoload 'ac-c-headers "ac-c-headers")
(add-hook 'c-mode-hook 'matt/c-indent)

;;------------------------------------------------------------------------------
;; C++
(add-hook 'c++-mode-hook
          (lambda ()
            (matt/c-indent)
            (c-set-offset 'statement-case-open 0)
            ;; don't indent curly for if...
            (c-set-offset 'substatement-open 0)
            ;; don't indent curly for inline method def
            (c-set-offset 'inline-open 0)))

;;------------------------------------------------------------------------------
;; Java
;(autoload 'jtags-mode "jtags" "Toggle jtags mode." t)
;(require 'eclim)
;(require 'eclimd)


;; (defvar eclimd-port nil
;;   "The active eclimd port number")

(custom-set-variables
  '(eclim-eclipse-dirs '("/Applications/eclipse"))
  '(eclim-executable "/Applications/eclipse/eclim"))

;; add the emacs-eclim source
;; (require 'ac-emacs-eclim-source)
;; (ac-emacs-eclim-config)
;
(defun matt/java-hooks ()
  ;; Treat Java 1.5 @-style annotations as comments.
  (setq c-comment-start-regexp "(@|/(/|[*][*]?))")
  (modify-syntax-entry ?@ "< b" java-mode-syntax-table)

  (jtags-mode)
 ; (eclim-mode)
;  (setq eclim-auto-save t)

  ;; Don't newline open curly
  (setq c-hanging-braces-alist
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
                'c-hanging-braces-alist)))


(add-hook 'java-mode-hook 'matt/java-hooks)
(add-hook 'java-mode-hook 'matt/c-indent)
(add-hook 'java-mode-hook (lambda ()  (c-set-offset 'substatement-open 0)))

(provide 'matt-prog-cc)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; matt-prog-cc.el ends here
