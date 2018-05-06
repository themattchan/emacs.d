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

 x-select-enable-clipboard t            ; integrate with the system clipboard
 initial-major-mode 'fundamental-mode
 undo-limit 1000
 fill-column 80                         ; 80 char columns
 ispell-dictionary "english"

 ;; Tabs and indentation and whitespace
 indent-tabs-mode nil                   ; tabs to spaces by default
 tab-width 2                            ; Default tab display is 2 spaces

 ;; default insert is also 4 and inc of 4
 ;; got to specify this or it will continue to expand to 8 spc
 ;; tab-stop-list (number-sequence 4 120 4)
 ;; highlight the whole expression when closing par ens
 ;; show-paren-style 'expression
 show-paren-delay 0

 next-line-add-newlines nil             ; No newlines at end of buffer unless I hit return
 sentence-end-double-space nil          ; sentences end with one space only.

 ;; FORCE FILES TO BE UTF-8 and LF
 buffer-file-coding-system 'utf-8-unix
 default-file-name-coding-system 'utf-8-unix
 default-keyboard-coding-system 'utf-8-unix
 default-process-coding-system '(utf-8-unix . utf-8-unix)
 default-sendmail-coding-system 'utf-8-unix
 default-terminal-coding-system 'utf-8-unix

 ;; flyspell
 flyspell-issue-welcome-flag nil
 ispell-list-command "list"

 ;; auto reload TAGS
 tags-revert-without-query 1
 )

;;------------------------------------------------------------------------------
;;(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;------------------------------------------------------------------------------
;; File formatting. yuck crlf
(prefer-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)

;;------------------------------------------------------------------------------
;; flyspell - aspell is better
(setq-default ispell-program-name
              (if *is-mac* "/usr/local/bin/aspell" "/usr/bin/aspell"))

(set-language-environment "UTF-8")

;;------------------------------------------------------------------------------
;; line numbers
(use-package linum
  :diminish
  :defer t
  :config
  (setq linum-format "%d "))

;;------------------------------------------------------------------------------
;; unto tree (what is this...)
(use-package undo-tree
  :diminish
  :defer t
  :config
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t)

  (global-undo-tree-mode))

;;------------------------------------------------------------------------------
;; yasnippet
;; load before auto-complete, so TAB expands snippet before completing
(use-package yasnippet
  :diminish (yas-minor-mode . "")
  :defer t
  :bind
  (:map yas-minor-mode-map
        ("<tab>" . nil)
        ("TAB" . nil)
        ;; use shift-tab for yasnippet completion so it doesn't conflict w/ ac-mode
        ("<S-tab>" . yas-expand))

  :config
  (setq yas-prompt-functions '(yas-ido-prompt yas-dropdown-prompt))
  (yas-global-mode 1)
  )


;;------------------------------------------------------------------------------
;; company mode
(use-package company
  :diminish
  :defer 10
  :config
  (global-company-mode))
;;(add-hook 'after-init-hook 'global-company-mode)
;(eval-after-load 'company-mode (lambda () (add-to-list 'company-backends 'company-ghc)))

(eval-and-compile
  (defun load-project-tags ()
    (interactive)
    (condition-case nil
        (let ((root (projectile-project-root)))
          (visit-tags-table (s-concat root "TAGS")))
      (error nil))))

(provide 'matt-edit-global)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; matt-edit-global.el ends here
