;;; matt-edit-global.el ---  Defaults for editing everything.

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

(setq-default
 ;; Backups
 backup-directory-alist '(("*"   . "~/.saves") ; don't litter my fs tree
                          (".*"  . "~/.saves")
                          ("*.*" . "~/.saves"))
 make-backup-files t    ; backup of a file the first time it is saved.
 backup-by-copying t    ; don't clobber symlinks
 version-control t      ; use versioned backups
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 auto-save-default t    ; auto-save every buffer that visits a file
 auto-save-timeout 60   ; number of seconds idle time before auto-save (default: 30)
 auto-save-interval 200 ; number of keystrokes between auto-saves (default: 300)

 ;; GLOBAL EDITING SETTINGS
 ;;

 x-select-enable-clipboard t            ; integrate with the system clipboard ffs
 initial-major-mode 'fundamental-mode   ; set initial major mode to be text
 undo-limit 1000                        ; Increase number of undo
 fill-column 80                         ; default fill-column is 80 chars
 ispell-dictionary "english"            ; English spelling, thanks

 ;; Tabs and indentation and whitespace
 ;; indent-tabs-mode nil                ; tabs to spaces by default
 tab-width 4                            ; Default tab display is 4 spaces
 ;; default insert is also 4 and inc of 4
 ;; got to specify this or it will continue to expand to 8 spc
 ;; tab-stop-list (number-sequence 4 120 4)
 ;; highlight the whole expression when closing par ens
 ;; show-paren-style 'expression

 next-line-add-newlines nil             ; No newlines at end of buffer unless I hit return
 sentence-end-double-space nil          ; sentences end with one space only.

 ;; FORCE FILES TO BE UTF-8 and LF damn it
 buffer-file-coding-system 'utf-8-unix
 default-file-name-coding-system 'utf-8-unix
 default-keyboard-coding-system 'utf-8-unix
 default-process-coding-system '(utf-8-unix .   utf-8-unix)
 default-sendmail-coding-system 'utf-8-unix
 default-terminal-coding-system 'utf-8-unix

 ;; flyspell
 flyspell-issue-welcome-flag nil
 ispell-list-command "list"
 )

;;------------------------------------------------------------------------------
;; Fuck off, whitespace
;;(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;------------------------------------------------------------------------------
;; File formatting. yuck crlf
(prefer-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)

;;------------------------------------------------------------------------------
;; flyspell - aspell is better
(if *is-mac*
    (setq-default ispell-program-name "/opt/local/bin/aspell")
  (setq-default ispell-program-name "/usr/bin/aspell"))

(set-language-environment "UTF-8")

;;------------------------------------------------------------------------------
;; line numbers
(autoload 'linum "linum-mode")
(eval-after-load "linum"
  ;; always one space separation, even in terminal
  (setq linum-format "%d "))

;;------------------------------------------------------------------------------
;; unto tree (what is this...)
(autoload 'undo-tree "undo-tree")
(global-undo-tree-mode)
(setq undo-tree-visualizer-timestamps t)
(setq undo-tree-visualizer-diff t)

;;------------------------------------------------------------------------------
;; yasnippet
;; load before auto-complete, so TAB expands snippet before completing
;;(require 'yasnippet)
(yas-global-mode 1)
(setq yas-prompt-functions '(yas-ido-prompt yas-dropdown-prompt))

;; use shift-tab for yasnippet completion so it doesn't conflict w/ ac-mode
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "<backtab>") 'yas-expand)

;;------------------------------------------------------------------------------
;; auto-complete mode
(require 'auto-complete)
(require 'auto-complete-config)

(defvar my-default-ac-sources
  ;; list of all sources for reference
  '(;; ac-source-abbrev
    ;; ac-source-dictionary
    ac-source-eclim
    ;; ac-source-features
    ;; ac-source-files-in-current-dir
    ac-source-functions
    ;; ac-source-gtags
    ;; ac-source-imenu
    ;; ac-source-ispell
    ;; ac-source-rcodetools
    ;; ac-source-semantic
    ;; ac-source-symbols
    ;; ac-source-variables
    ;; ac-source-words-in-all-buffer
    ac-source-words-in-buffer
    ;; ac-source-words-in-same-mode-buffers
    ;; ac-source-yasnippet
    ac-source-filename
    ))

(setq-default ac-sources my-default-ac-sources)

(defun ac-text-mode-setup ()
  (setq ac-sources (append
                    '(ac-source-dictionary
                      ac-source-ispell)
                    ac-sources)))

(add-hook 'text-mode-hook 'ac-text-mode-setup)

(defun ac-prog-mode-setup ()
  (setq ac-sources (append
                    '(;; ac-source-words-in-same-mode-buffers
                      ac-source-files-in-current-dir)
                    ac-sources)))

(add-hook 'prog-mode-hook 'ac-prog-mode-setup)

(defun ac-emacs-lisp-mode-setup ()
  (setq ac-sources (append
                    '(ac-source-features
                      ac-source-functions
                      ac-source-variables
                      ac-source-symbols) ac-sources))

  (setq ac-omni-completion-sources
        '(("\\<featurep\s+'" ac+-source-elisp-features)
          ("\\<require\s+'"  ac+-source-elisp-features)
          ("\\<load\s+\""    ac-source-emacs-lisp-features))))

(add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)

(defun ac-c-mode-setup ()
  (setq ac-sources (append '(ac-source-gtags ac-source-semantic) ac-sources))
  (setq ac-omni-completion-sources (cons "\\." '(ac-source-semantic)))
  (setq ac-omni-completion-sources (cons "\\->" '(ac-source-semantic))))

(add-hook 'c-mode-common-hook 'ac-c-mode-setup)

(setq ac-auto-start 3
      ;; ac-auto-show-menu 0.1
      ac-use-menu-map t
      ac-expand-on-auto-complete t
      ac-use-quick-help t
      ac-dwim t) ; To get pop-ups with docs even if a word is uniquely completed

;; Keybindings
(ac-set-trigger-key "TAB")
(define-key ac-completing-map (kbd "C-n") 'ac-next)
(define-key ac-completing-map (kbd "C-p") 'ac-previous)
;; (define-key ac-completing-map [return] nil)
(define-key ac-completing-map "\t" 'ac-complete)
(define-key ac-completing-map "\r" nil)

(ac-config-default)

(add-to-list 'ac-modes 'geiser-repl-mode)
(add-to-list 'ac-modes 'LaTeX-mode)

;(global-auto-complete-mode t)           ; turn it on

;; (eval-after-load "auto-complete"
;;   '(progn
;;      (ac-ispell-setup)))

;;------------------------------------------------------------------------------
;; company mode

(add-hook 'after-init-hook 'global-company-mode)
;(eval-after-load 'company-mode (lambda () (add-to-list 'company-backends 'company-ghc)))

(provide 'matt-edit-global)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; matt-edit-global.el ends here
