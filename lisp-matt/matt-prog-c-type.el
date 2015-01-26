;; C family common customizations
;; ------------------------------
;;
;; cc-mode hooks in order:
;; 1. c-initialization-hook, init cc mode once per session (i.e. emacs startup)
;; 2. c-mode-common-hook, run immediately before loading language hook
;; 3. then language hooks:
;;    c, c++, objc, java, idl, pike, awk

(defun matt/c-indent ()
   ;; set correct backspace behaviour
   ;; c-backspace-function 'backward-delete-char
   ;; c-type lang specifics. want 4-space width tab tabs
    (setq-local c-basic-offset 4)
   ;; (setq-local c-indent-level 4)
    ;; (setq-local c-indent-tabs-mode nil)               ; tabs please
    (setq-local indent-tabs-mode nil)   ; nil is spaces
    (setq-local c-tab-always-indent nil)
    (setq-local tab-width 4)
    ;;(setq-local tab-stop-list (number-sequence 8 120 8))
    )

(add-hook 'asm-mode-hook
          (lambda ()
            (auto-complete-mode 0)
            (setq-local asm-comment-char ?\!)
            (setq-local tab-width 8)
            (setq-local tab-stop-list (number-sequence 8 120 8))
            (setq-local indent-tabs-mode t)))

(add-hook 'c-initialization-hook
          (lambda ()
            (matt/c-indent)             ; just to be sure
            (setq-default c-default-style '((c-mode    . "linux")
                                            (c++-mode  . "k&r")
                                            (java-mode . "java")
                                            (awk-mode  . "awk")
                                            (other     . "free-group-style")))
            (add-to-list 'c-cleanup-list 'comment-close-slash)))

(add-hook 'c-mode-common-hook
          (lambda ()
            (matt/c-indent)
            ;; (setq show-paren-style 'expression) ; highlight blocks
            (c-toggle-electric-state 1)
            (c-toggle-auto-newline 1)
            ;; subword editing and movement to deal with CamelCase
            (subword-mode 1)
            (electric-pair-mode 1)
            ;; don't indent curly braces. gnu style is madness.
            (c-set-offset 'statement-case-open 0)
            (c-set-offset 'substatement-open 0)
            (c-set-offset 'comment-intro 0)))

;; C
(autoload 'ac-c-headers "ac-c-headers")
(add-hook 'c-mode-hook
          (lambda ()
            (matt/c-indent)
            (add-to-list 'ac-sources 'ac-source-c-headers)
            (add-to-list 'ac-sources 'ac-source-c-header-symbols t)))

;; Java
(autoload 'jtags-mode "jtags" "Toggle jtags mode." t)

(defun matt/java-hooks ()
  "Treat Java 1.5 @-style annotations as comments."
  (setq c-comment-start-regexp "(@|/(/|[*][*]?))")
  (modify-syntax-entry ?@ "< b" java-mode-syntax-table)
  (jtags-mode))

(add-hook 'java-mode-hook 'matt/java-hooks)
(add-hook 'java-mode-hook 'matt/c-indent)

(provide 'matt-prog-c-type)
