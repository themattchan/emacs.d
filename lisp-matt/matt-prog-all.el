;;==============================================================================
;; PROGRAMMING MODES
;;==============================================================================
(provide 'matt-prog-all)

;; highlight 80+ char overflows in programming modes
(autoload 'whitespace-mode "whitespace" "whitespace-mode" t nil)
;; (add-hook 'prog-mode-hook 'whitespace-mode)

;; highlights, line numbers, etc, common to all programming modes
;;(add-hook 'prog-mode-hook 'ac-ispell-ac-setup)

;;(autoload 'rainbow-delimiters "rainbow-delimiters" nil t)
;;(add-hook 'prog-mode-hook 'rainbow-delimiters-autoloads)

(add-hook 'prog-mode-hook
          (lambda()
            ;;(matt/load-theme 'adwaita)
            (electric-indent-mode 1)    ; auto indent
            (linum-mode 1)
            (show-paren-mode 1)
            (hl-line-mode 1)            ; highlight current line
            (auto-fill-mode 1)
            (flyspell-prog-mode)
            (flycheck-mode)
            (whitespace-mode)
            (subword-mode)

            ;; (define-key ac-mode-map (kbd "<backtab>") 'auto-complete)
            (setq
             ;; tabs are tabs when i'm programming, unless specified
             indent-tabs-mode t
             ;; also, ensure that tabs are 4 spc wide unless specified
             tab-width 4

             flycheck-check-syntax-automatically '(mode-enabled save idle-change)
             flycheck-completion-system 'ido
             show-paren-delay 0
             ;; enable multiline comments
             comment-multi-line t
             comment-auto-fill-only-comments t
             grep-highlight-matches t   ; grep in colour
             )))

;;------------------------------------------------------------------------------
;; flycheck

(eval-after-load 'flycheck
  (lambda ()
    ;; pos-tip on click
    (setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages)
    (setq flycheck-highlighting-mode 'symbols) ; 'lines is faster than 'sexps
    (setq flycheck-display-errors-delay 2)   ; seconds

    (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)

    ;; colours at http://raebear.net/comp/emacscolors.html
    (set-face-attribute 'flycheck-error nil
                        :foreground "red"
                        :background "pink"
                        :underline "red")

    (set-face-attribute 'flycheck-warning nil
                        :foreground "black"
                        :background "cornsilk"
                        :underline "maroon")

    (set-face-attribute 'flycheck-info nil
                        ;; info highlights are annoying as hell
                        ;; :foreground "blue4"
                        ;; :background "LightBlue1"
                        :underline "ForestGreen")
    ))

;;------------------------------------------------------------------------------
;; No auto-indent
(let ((whitespace-langs '(python-mode
                          yaml-mode
                          haskell-mode
                          literate-haskell-mode)))
  (dolist (mode whitespace-langs)
    (add-hook (intern (format "%s-hook" mode))
              (lambda ()
                (electric-indent-mode -1)
                (electric-pair-mode -1)))))

;;------------------------------------------------------------------------------
;; whitespace-mode
(defvar only-trailing-whitespace-style '(face lines-tail))
(add-hook 'prog-mode-hook
          (lambda()
            (setq
             ;; highlight 80 char overflows
             whitespace-line-column fill-column
             whitespace-style only-trailing-whitespace-style)))

(setq whitespace-display-mappings
      '((space-mark   32 [183] [46])
        (newline-mark 10 [182 10])      ; pilcrow
        (tab-mark     9  [9655 9 ] [92 9])))

;; adapted from  https://github.com/expez/.emacs.d/blob/master/init-whitespace.el
(defvar whitespace-show-all-mode nil)
(defun* toggle-whitespace-mode ()
  "Toggles whitespace modes between modes where some whitespace
   is highligted and all whitespace is highlighted.
   With a prefix argument whitespace-mode is turned off.
   C-u <prefix> M-x toggle-whitespace-mode"

  (interactive)
  (when current-prefix-arg
    (if whitespace-mode
        (progn
          (whitespace-mode 0)
          (message "Whitespace mode off"))
      (whitespace-mode 1)
      (message "Whitespace mode on"))
    (return-from toggle-whitespace-mode))

  (if whitespace-show-all-mode
      (progn
        (setq whitespace-style only-trailing-whitespace-style)
        (setq whitespace-show-all-mode nil)
        (whitespace-mode 0)
        (whitespace-mode 1)
        (message "Highlighting overflow only"))
    (setq whitespace-style
          '(face tabs spaces trailing lines-tail space-before-tab newline
                 indentation empty space-after-tab space-mark tab-mark
                 newline-mark))
    (setq whitespace-show-all-mode t)
    (whitespace-mode 0)
    (whitespace-mode 1)
    (message "Highlighting all whitespace")))

(global-set-key (kbd "C-x t w") 'toggle-whitespace-mode)

;;------------------------------------------------------------------------------
;; column width
;; ring of line lengths
(defvar line-lens (make-ring 5))
(mapc (lambda (obj) (ring-insert line-lens obj))
      (reverse '(80 100 120)))          ; rotate properly

;; rotate through line lengths
(defun toggle-line-length ()
  "Change from 80 to 100 chars and v.v."
  (interactive)
  (let ((next-line-len (ring-ref line-lens
                                 (+ 1 (ring-ref line-lens fill-column)))))
      (set-fill-column next-line-len)
      (setq whitespace-line-column next-line-len)
      (whitespace-mode 0)
      (whitespace-mode 1)))

(global-set-key (kbd "C-x t l") 'toggle-line-length)

;;------------------------------------------------------------------------------
;; auto guess tabs or spaces
;; (autoload 'prog-mode "guess-style" "guess-style.el." t)
;;  (add-hook 'prog-mode-hook guess-style-guess-tabs-mode)
;;    (add-hook 'prog-mode-hook (lambda ()
;;                                   (when indent-tabs-mode
;;                                     (guess-style-guess-tab-width)))

;; compilation
;; Get compilation to understand javac/ant errors
;; (eval-after-load "compile"
;;   '(setq compilation-error-regexp-alist
;;          (append (list
;;                   ;; works for jikes
;;                   '("^\\s-*\\[[^]]*\\]\\s-*\\(.+\\):\\([0-9]+\\):\\([0-9]+\\):[0-9]+:[0-9]+:" 1 2 3)
;;                   ;; works for javac
;;                   '("^\\s-*\\[[^]]*\\]\\s-*\\(.+\\):\\([0-9]+\\):" 1 2))
;;                  compilation-error-regexp-alist)))

;;(global-set-key (kbd "TAB") 'self-insert-command)

;;------------------------------------------------------------------------------
;; ctags
;; doesn't work on mac, xcode hijacks the path to macports ctags
;; (defun create-tags (dir-name)
;;   "Create tags file."
;;   (interactive "Directory: ")
;;   (shell-command
;;    (format "/opt/local/bin/ctags -e -R %s" (directory-file-name dir-name)))
;;   )

;;------------------------------------------------------------------------------
;; use etags
;; (defun create-tags (dir-name)
;;   "Create tags file."
;;   (interactive "Directory: ")
;;   (eshell-command
;;    (format "find %s -type f -name \"*.[ch]\" | etags -" dir-name)))

;;------------------------------------------------------------------------------
;; Emacs code browser
(require 'ecb)

;;; activate and deactivate ecb
(setq ecb-show-sources-in-directories-buffer 'always)
(setq ecb-compile-window-height 12)
(global-set-key (kbd "C-x C-;") 'ecb-activate)
(global-set-key (kbd "C-x C-'") 'ecb-deactivate)
;;; show/hide ecb window
(global-set-key (kbd "C-;") 'ecb-show-ecb-windows)
(global-set-key (kbd "C-'") 'ecb-hide-ecb-windows)
;;; quick navigation between ecb windows
(global-set-key (kbd "C-)") 'ecb-goto-window-edit1)
(global-set-key (kbd "C-!") 'ecb-goto-window-directories)
(global-set-key (kbd "C-@") 'ecb-goto-window-sources)
(global-set-key (kbd "C-#") 'ecb-goto-window-methods)
(global-set-key (kbd "C-$") 'ecb-goto-window-compilation)
