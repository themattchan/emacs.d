;;; matt-interface.el --- Startup, UI, and general settings.

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

;;------------------------------------------------------------------------------
;; Mac open new files in the existing frame
(setq ns-pop-up-frames nil)

;;------------------------------------------------------------------------------
;; Show time on the mode line
(display-time)

;;------------------------------------------------------------------------------
;; Kill UI cruft
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
;; Want menu bar only in GUIs
(cond ((not window-system)
       (menu-bar-mode -1)))

;; No cursor blink
(blink-cursor-mode -1)

;;------------------------------------------------------------------------------
;; Load stuff
(eval-when-compile (require 'cl))
(autoload 'saveplace "save last loc in file")
(autoload 'uniquify "unique buffer titles")

;; dired jump to current file dir (C-x C-j)
(autoload 'dired-x "dired-x")

;;------------------------------------------------------------------------------
;; Smart mode line
(autoload 'smart-mode-line "sml")
(sml/setup)
;; '(:eval (if (use-region-p)
;;     (format "%d"
;;       (count-words-region (point) (mark)))
;;   (format "%d"
;;     (count-words-region (point-min) (point-max)))))
;; show battery
;; (add-hook 'after-init-hook #'fancy-battery-mode)
;; (fancy-battery-mode)
;; (setq-default fancy-battery-show-percentage t)
;; Start the server
;; (server-start)

;;------------------------------------------------------------------------------
;; Completion modes, etc
;; Use ibuffer to list buffers by default
(defalias 'list-buffers 'ibuffer)

;; isearch buffer switching
(icomplete-mode 1)

;; helm-mode
;; From Bryans's config
;; https://github.com/bryangarza/dot-emacs/blob/master/bryan/bryan-helm.el
(use-package helm
  :init
  (progn
    (require 'helm-config)
    (when (executable-find "curl")
      (setq helm-net-prefer-curl t))
    (setq
     helm-input-idle-delay                 0.001
     helm-idle-delay                       0.001
     helm-candidate-number-limit           100
     helm-autoresize-max-height            40 ; it is %.
     helm-scroll-amount                    8
     helm-split-window-in-side-p           t
     helm-move-to-line-cycle-in-source     t
     helm-ff-search-library-in-sexp        t
     helm-ff-file-name-history-use-recentf t
     helm-quick-update                     t
     helm-bookmark-show-location           t
     ;; fuzzy match
     helm-M-x-fuzzy-match                  t
     helm-ag-fuzzy-match                   t
     helm-buffers-fuzzy-matching           t
     helm-apropos-fuzzy-match              t
     helm-recentf-fuzzy-match              t
     helm-locate-fuzzy-match               t
     helm-file-cache-fuzzy-match           t
     helm-semantic-fuzzy-match             t
     helm-imenu-fuzzy-match                t
     helm-lisp-fuzzy-completion            t)

    (helm-mode)
    (add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)
    (ido-mode -1) ; just in case
    (helm-autoresize-mode t)

    (custom-set-variables
     '(helm-ag-base-command "ag --nocolor --nogroup --ignore-case")
     '(helm-ag-command-option "--all-text")
     '(helm-ag-insert-at-point 'symbol))

    (global-unset-key (kbd "C-x c"))

    (bind-keys :map helm-map
     ("<tab>" . helm-execute-persistent-action) ; rebind tab to run persistent action
     ("C-i"   . helm-execute-persistent-action) ; make TAB works in terminal
     ("C-z"   . helm-select-action)) ; list actions using C-z
    (bind-key "C-c C-l" 'helm-comint-input-ring shell-mode-map)
    (bind-key "C-c C-l" 'helm-minibuffer-history minibuffer-local-map))

  :bind
  (;; help
   ("C-h a"     . helm-apropos)
   ("C-h i"     . helm-info-emacs)
   ("C-h b"     . helm-descbinds)
   ("s-m"       . helm-man-woman)

   ;; super aliases
   ("s-f"       . helm-find-files)
   ("s-b"       . helm-mini)
   ("s-F"       . helm-occur)

   ;; rebind common keys, searching + browsing
   ("C-c h"     . helm-command-prefix)
   ("M-x"       . helm-M-x)
   ("M-y"       . helm-show-kill-ring)
   ("C-x b"     . helm-mini)
   ("C-x C-f"   . helm-find-files)
   ("C-x f"     . helm-recentf)
   ("C-x C-d"   . helm-browse-project)

   ("C-h SPC"   . helm-all-mark-rings)
   ("C-c h M-:" . helm-eval-expression-with-eldoc)
   ("C-c h o"   . helm-occur)

   ;; silver searcher
   ("M-s s"     . helm-ag)
   ("M-s a"     . helm-ag-project-root)
   ("M-s b"     . helm-ag-buffers)
   ("M-s p"     . helm-ag-pop-stack)
   ("M-s C-c"   . helm-ag-clear-stack)
   ))


;; DEPRECATED
;; ;; ido
;; (autoload 'ido "ido")
;; ;(autoload 'flx-ido "flx-ido")
;; (ido-mode 'both)
;; (ido-everywhere 1)
;; ;(flx-ido-mode 1)

;; (setq
;;  ido-save-directory-list-file "~/.emacs.d/cache/ido.last"

;;  ido-ignore-buffers ;; ignore these guys
;;  '("\\` " "^\*Mess" "^\*Back" ".*Completion" "^\*Ido" "^\*trace"
;;    "^\*compilation" "^\*GTAGS" "^session\.*" "^\*")
;;  ido-work-directory-list '("~/" "~/Desktop" "~/Documents" "~src")
;;  ido-case-fold  t                    ; be case-insensitive

;;  ido-enable-last-directory-history t ; remember last used dirs
;;  ido-max-work-directory-list 30      ; should be enough
;;  ido-max-work-file-list      50      ; remember many
;;  ido-use-filename-at-point nil       ; don't use filename at point (annoying)
;;  ido-use-url-at-point nil            ; don't use url at point (annoying)

;;  ido-enable-flex-matching nil        ; don't try to be too smart
;;  ido-max-prospects 8                 ; don't spam my minibuffer
;;  ido-confirm-unique-completion t     ; wait for RET, even with unique completion
;;  ido-use-virtual-buffers t
;;  ido-use-faces nil
;;  )

;; when using ido, the confirmation is rather annoying...
 (setq confirm-nonexistent-file-or-buffer nil)
;; icicles tab completion
;; (autoload 'icicles "icicles")
;; (icicle-mode t)

;;------------------------------------------------------------------------------
;; winner-mode
(winner-mode 1)

;;------------------------------------------------------------------------------
;; Recent files
(autoload 'recentf "recentf")
(recentf-mode 1)
(setq recentf-max-menu-items 30)
;;(bind-key "C-x f" 'recentf-open-files)  ; rebind fill column, use M-x
                                        ; set-fill-column

;;------------------------------------------------------------------------------
(autoload 'sr-speedbar "sr-speedbar") ;; in-window-speedbar

;;------------------------------------------------------------------------------
;; Fix Emacs interface annoyances
(setq-default                     ; Global settings for ALL BUFFERS
 ;; kill the splash screen and all that garbage
 inhibit-splash-screen t
 inhibit-startup-message t
 inhibit-startup-screen t
 inhibit-startup-buffer-menu t
 initial-scratch-message ""
 ;;inhibit-startup-echo-area-message "matt"
 ;;initial-buffer-choice                    ; Open to some file
 menu-prompting nil
 confirm-kill-emacs 'y-or-n-p ; Ask before exit - takes a while to load, y'know?

 ;;frame-title-format '(buffer-file-name "%f" "%b") ; Full file path in title
 display-warning-minimum-level 'error   ; Turn off annoying warning messages
 disabled-command-function nil          ; Don't second-guess advanced commands

 ;; mode line customizations
 display-battery-mode t
 battery-mode-line-format " [%L: %b%p%%] " ; %t for time

 line-number-mode t
 column-number-mode t


 ;; square cursor
 cursor-type 'box
 ;; scrolling does not move cursor
 scroll-preserve-screen-position t
 ;; use wheel
 mouse-wheel-mode t
 echo-keystrokes 0.1


 redisplay-dont-pause t

 ;; buffer handling
 save-place t
 save-place-forget-unreadable-files t
 uniquify-rationalize-file-buffer-names t
 uniquify-buffer-name-style 'forward
 buffers-menu-sort-function 'sort-buffers-menu-by-mode-then-alphabetically ; Buffers menu settings
 buffers-menu-grouping-function 'group-buffers-menu-by-mode-then-alphabetically
 buffers-menu-submenus-for-groups-p t
 ibuffer-default-sorting-mode 'filename/process

 ;; font lock
 font-lock-use-fonts '(or (mono) (grayscale))    ; Maximal syntax highlighting
 font-lock-use-colors '(color)
 font-lock-maximum-decoration t
 font-lock-maximum-size nil
 font-lock-auto-fontify t

 ;; stfu and stop beeping. you ain't vim.
 ring-bell-function 'ignore
 ) ;; end startup setq

;; Redefine startup messasge
(defun startup-echo-area-message ()
  "By your command...")

;; y/n prompts instead of yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

;; normal delete key behaviour please
;; highlight selection and overwrite
(delete-selection-mode t)
(transient-mark-mode t)

;;------------------------------------------------------------------------------
;; smart parens nav
;; (smartparens-global-mode t)

;;------------------------------------------------------------------------------
;; Fonts (face) customization
(autoload 'faces "faces")
;; default font size is 14pt on carbon emacs
(when (window-system)
  (cond
   (*is-mac*
    (set-face-attribute 'default nil
                        :font "Monaco"
                        :height 160    ; default font size is 12pt on carbon emacs
                        :weight 'normal
                        :width 'normal))

   (*is-linux*
    (set-face-attribute 'default nil
                        :font "Monospace-13"
                        :height 150
                        :weight 'normal
                        :width 'normal))

   (*is-windows*
    (set-face-attribute 'default nil
                        :font "Lucida Console 10" ; Consolas is also good
                        :weight 'normal))))

;;------------------------------------------------------------------------------
;; No popups and dialogues. They crash carbon emacs.
;; Not to mention that they're incredibly annoying.
(defadvice y-or-n-p (around prevent-dialog activate)
  "Prevent y-or-n-p from activating a dialog"
  (let ((use-dialog-box nil))
    ad-do-it))
(defadvice y-or-n-p (around prevent-dialog-yorn activate)
  "Prevent y-or-n-p from activating a dialog"
  (let ((use-dialog-box nil))
    ad-do-it))
;; Fallback. DIE, DIALOGUE BOXES, DIE!!
(setq use-dialog-box nil)

;;------------------------------------------------------------------------------
;; Smooth scrolling
(setq scroll-margin 1
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1
      mouse-wheel-follow-mouse 't
      mouse-wheel-scroll-amount '(1 ((shift) . 1)))

;; because sometimes you want a mouse
;; (autoload 'xt-mouse "xt-mouse")
;; (xterm-mouse-mode)

;;------------------------------------------------------------------------------
;; M-x aliases
(defalias 'ct 'load-theme)
(defalias 'sb 'sr-speedbar-toggle)
(defalias 'rs 'replace-string)
(defalias 'rex 'replace-regexp)

;;------------------------------------------------------------------------------
;; Unclutter mode line
(autoload 'diminish "diminish")

(defvar mode-line-cleaner-alist
  `((auto-complete-mode         . " α")
    (yas/minor-mode             . " υ")
    (paredit-mode               . " π")
    (eldoc-mode                 . ""  )
    (abbrev-mode                . ""  )
    (smartparens-mode           . ""  )
    (undo-tree-mode             . ""  )
    (auto-highlight-symbol-mode . ""  )
    (whitespace-global-mode     . ""  )
    (whitespace-mode            . ""  )
    (auto-fill-mode             . ""  )
    (flyspell-mode              . ""  )
    (flycheck-mode              . ""  )
    (subword-mode               . ""  )
    (yas-minor-mode             . ""  )
    (helm-mode                  . ""  )
    (company-mode               . ""  )
    (projectile-mode            . ""  )

    ;; Major modes
    (lisp-interaction-mode      . "λeval" )
    (lisp-mode                  . "(())"  )
    (scheme-mode                . "(λ)"   )
    (racket-mode                . "(λr)"  )
    (clojure-mode               . "(λclj)")
    (emacs-lisp-mode            . "(λel)" )
    (common-lisp-mode           . "(λcl)" )

    (haskell-mode               . "λ"     )
    (tuareg-mode                . "λOCaml")

    (hi-lock-mode               . ""  )
    (python-mode                . "py")
    (nxhtml-mode                . "nx"))
  "Alist for 'clean-mode-line'.

When you add a new element to the alist, keep in mind that you
must pass the correct minor/major mode symbol and a string you
want to use in the modeline *in lieu of* the original.")

(defun clean-mode-line ()
  (interactive)
  (loop for cleaner in mode-line-cleaner-alist
        do (let* ((mode (car cleaner))
                 (mode-str (cdr cleaner))
                 (old-mode-str (cdr (assq mode minor-mode-alist))))
             (when old-mode-str
                 (setcar old-mode-str mode-str))
               ;; major mode
             (when (eq mode major-mode)
               (setq mode-name mode-str)))))
(add-hook 'init-hook 'clean-mode-line)
(add-hook 'after-change-major-mode-hook 'clean-mode-line)

;;------------------------------------------------------------------------------
;; Projectile mode by default
(projectile-global-mode)


(provide 'matt-interface)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; matt-interface.el ends here
