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
  (setq c-backspace-function 'backward-delete-char)

  ;; c-type lang specifics. want 4-space width tab tabs
  (setq c-basic-offset 4)
  ;; (setq-local c-indent-level 4)

  (setq c-indent-tabs-mode t)           ; tabs please
  (setq c-tab-always-indent t)          ; t for tabs, nil for spaces
  ;;(setq indent-tabs-mode t)
  (setq tab-width 4)
  ;;(setq-local tab-stop-list (number-sequence 8 120 8))

  ;; don't indent curly braces. gnu style is madness.
  ;; (c-set-offset 'statement-case-open 0)
  ;; (c-set-offset 'substatement-open 0)
  ;; (c-set-offset 'comment-intro 0)
  ;; (c-set-offset 'brace-list-intro 0)
  )


(add-hook 'asm-mode-hook
          (lambda ()
            (auto-complete-mode 0)
            (setq-local asm-comment-char ?\!)
            (setq-local tab-width 8)
            (setq-local tab-stop-list (number-sequence 8 120 8))
            (setq-local indent-tabs-mode t)))

;; (add-hook 'c-initialization-hook )

(add-hook 'c-mode-common-hook
          (lambda ()
            (matt/c-indent)
            (add-to-list 'c-cleanup-list 'comment-close-slash)
            (setq c-default-style '((c-mode    . "linux")
                                    (c++-mode  . "stroustrup")
                                    (java-mode . "java")
                                    (awk-mode  . "awk")
                                    (other     . "free-group-style")))
            ;; (setq show-paren-style 'expression) ; highlight blocks
            (c-toggle-electric-state 1)
            (c-toggle-auto-newline 1)
            ;; subword editing and movement to deal with CamelCase
            (subword-mode 1)
            (electric-pair-mode 1)))

;; C
(autoload 'ac-c-headers "ac-c-headers")
(add-hook 'c-mode-hook
          (lambda ()
            (matt/c-indent)
            (add-to-list 'ac-sources 'ac-source-c-headers)
            (add-to-list 'ac-sources 'ac-source-c-header-symbols t)))

;; C++
(add-hook 'c++-mode-hook
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
