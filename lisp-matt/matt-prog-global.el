;;; matt-prog-global.el --- Global settings for Programming modes.

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

;;; Code:

(use-package auto-fill
  :diminish
  :defer 5
  :commands auto-fill-mode
  :init
  (add-hook 'prog-mode-hook #'auto-fill-mode)
  (add-hook 'text-mode #'auto-fill-mode))

(use-package paren
  :diminish
  :demand t
  ;; :hook ((prog-mode . 'show-paren-mode)
  ;;        (text-mode . 'show-paren-mode))
  :config
  (show-paren-mode 1))

(add-hook 'makefile-mode (lambda () (setq indent-tabs-mode t)))

(add-hook 'prog-mode-hook
          (lambda()
            ;;(matt/load-theme 'adwaita)
            (electric-indent-mode 1)    ; auto indent
;            (linum-mode 1)
;;            (show-paren-mode)
            (hl-line-mode 1)            ; highlight current line
;;            (auto-fill-mode 1)
            ;(flyspell-prog-mode)  ;; disabled, really slow...
           ;; (flycheck-mode)
           ;; (flycheck-pos-tip-mode)
;;            (whitespace-mode)
            (subword-mode)
            (fixmee-mode)
            ;; (define-key ac-mode-map (kbd "<backtab>") 'auto-complete)
            (setq
             ;; tabs are spaces unless specified
             indent-tabs-mode nil
             ;; also, ensure that tabs are 4 spc wide unless specified
             tab-width 4

             flycheck-completion-system 'ido
             show-paren-delay 0
             ;; enable multiline comments
             comment-multi-line t
             comment-auto-fill-only-comments t
             grep-highlight-matches t   ; grep in colour
             )))

;;------------------------------------------------------------------------------
;; flycheck

(use-package flycheck
  :diminish
  :ensure t
  :defer t
  :commands (flycheck-mode)
  :hook (prog-mode . flycheck-mode)
  :config
  ;; pos-tip on click
  (setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages)
  (setq flycheck-highlighting-mode 'symbols) ; 'lines is faster than 'sexps
  (setq flycheck-display-errors-delay 2)   ; seconds
  (setq flycheck-idle-change-delay 30) ; seconds
  (setq flycheck-check-syntax-automatically '(mode-enabled save idle-change))

  (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)
  (add-hook 'flycheck-mode-hook 'flycheck-pos-tip-mode)

  (setq-default flycheck-disabled-checkers
                '(javascript-jshint emacs-lisp-checkdoc))

  (flycheck-add-mode 'javascript-eslint 'web-mode)

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
  )

(use-package flycheck-pos-tip
  :diminish
  :defer t
  :after flycheck
  :config
  (flycheck-pos-tip-mode))

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
(use-package whitespace
  :diminish
  :defer t
  :bind (("C-x t w" . 'toggle-whitespace-mode))
  ;; :hook (prog-mode )
  :init
  (defvar only-trailing-whitespace-style '(face lines-tail))
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

  :config
  (setq
   ;; highlight 80 char overflows
   whitespace-line-column fill-column
   whitespace-style only-trailing-whitespace-style)


  (setq whitespace-display-mappings
        '((space-mark   32 [183] [46])
          (newline-mark 10 [182 10])      ; pilcrow
          (tab-mark     9  [9655 9 ] [92 9])))

  ;; adapted from  https://github.com/expez/.emacs.d/blob/master/lisp/init-whitespace.el

  ) ;; use-package whitespace

;;------------------------------------------------------------------------------
;; Language server protocol

(add-hook 'lsp-mode-hook 'lsp-ui-mode)
(add-hook 'purescript-mode #'lsp-purescript-enable)

;;------------------------------------------------------------------------------
;; Emacs code browser
(use-package ecb
  :defer t
  :bind
  (("C-x C-;" . ecb-activate)
   ("C-x C-'" . ecb-deactivate)
   ;; show/hide ecb window
   ("C-;" . ecb-show-ecb-windows)
   ("C-'" . ecb-hide-ecb-windows)
   ;; quick navigation between ecb windows
   ("C-)" . ecb-goto-window-edit1)
   ("C-!" . ecb-goto-window-directories)
   ("C-@" . ecb-goto-window-sources)
   ("C-#" . ecb-goto-window-methods)
   ("C-$" . ecb-goto-window-compilation))

  :config
  ;; activate and deactivate ecb
  (setq ecb-show-sources-in-directories-buffer 'always)
  (setq ecb-compile-window-height 12))

(provide 'matt-prog-global)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; matt-prog-global.el ends here
