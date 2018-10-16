;;; -*- lexical-binding: t -*-
;;; matt-utils.el --- Settings for various "app modes"

;;; Copyright (c) 2013-2016 Matthew Chan
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
;; Dired

(use-package dired
  :defer 15
  :config
  (require 'dired-details+)
  (require 'dired-x)
  ;; hide the -al stuff, toggle with '(' and ')'
  (setq dired-recursive-deletes 'always
        dired-recursive-copies 'always
        delete-by-moving-to-trash t
        trash-directory "~/.Trash/"
        ;; show sym link targets
        Dired-details-hide-link-targets nil)
  (add-hook 'dired-mode-hook 'turn-on-stripe-buffer-mode)
  ;; (add-hook 'dired-mode-hook 'stripe-listify-buffer)
  (setq-default ; C-x M-o to toggle omit mode
   dired-omit-mode t
   dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\.\\|^~")
  )


;;------------------------------------------------------------------------------
;; Terminal
;; (use-package multi-term
;;   :ensure t
;;   :commands (multi-term)
;;   :bind (("M-o M-t" . multi-term)
;;          ;; ("M-[" . multi-term-prev)
;;          ;; ("M-]" . multi-term-next))
;;          )
;;   :init
;;   (progn
;;     (setq multi-term-program "/bin/zsh")))
(use-package ansi-color
  :defer t
  :init
  (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on))
(use-package xterm-color
  :defer t
  :init
  (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter)
  :config
  (setq xterm-color-preserve-properties t))

                                        ;(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)

(setq explicit-shell-file-name (executable-find "zsh"))
(setq shell-command-switch "-ic")
(setq system-uses-terminfo nil)

(setenv "ESHELL" (executable-find "zsh"))
(custom-set-faces '(term ((t (:inherit default)))))

(use-package eshell
  :defer t
  :config
  (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
  (setq eshell-output-filter-functions (remove 'eshell-handle-ansi-color eshell-output-filter-functions)))

(eval-and-compile
  (defun new-shell ()
    (interactive)
    (projectile-with-default-dir (projectile-project-root)
      (shell (generate-new-buffer-name "*shell*")))))

(global-set-key (kbd "M-o M-t") #'new-shell)

;;------------------------------------------------------------------------------
;; TRAMP-mode
(use-package tramp
  :defer t
  :config
  (setq tramp-default-method "ssh"
        tramp-terminal-type "dumb"
        tramp-default-method-alist '())
  (add-to-list 'tramp-default-method-alist '("ieng9.ucsd.edu" "" "scpx"))
  (setenv "SHELL" "/bin/bash")
  (setq password-cache-expiry nil))

;;------------------------------------------------------------------------------
;; IRC
;; mostly from https://github.com/bryangarza/dot-emacs/blob/master/init.el#L1273


;; (use-package erc
;;   :disabled t
;;   :defer t
;;   :config

;;   (use-package erc-services
;;     :config
;;     (erc-services-mode 1))

;;   (use-package znc)

;;   (setq erc-server-coding-system '(utf-8 . utf-8)

;;         erc-prompt-for-nickserv-password nil

;;         erc-timestamp-format "[%I:%M %p]"
;;         erc-hide-timestamps t
;;         erc-echo-timestamps nil
;;         erc-echo-timestamp-format "TS'd %A, %I:%M:%S %p"

;;         erc-track-showcount t
;;         erc-track-enable-keybindings t
;;         erc-track-use-faces t
;;         erc-track-exclude-types '("JOIN" "PART" "QUIT" "NICK" "MODE")

;;         erc-join-buffer 'bury

;;         erc-part-reason-various-alist '(("^$" "Leaving"))
;;         erc-quit-reason-various-alist '(("^$" "Leaving"))
;;         erc-quit-reason 'erc-part-reason-various
;;         erc-part-reason 'erc-quit-reason-various

;;         erc-hide-list '("JOIN" "PART" "QUIT" "NICK" "MODE" "324" "329" "332" "333" "353"  "477")
;;         erc-lurker-hide-list '("JOIN" "PART" "QUIT" "NICK" "MODE")

;;         erc-autojoin-channels-alist
;;         '(("freenode.net"
;;            "#haskell" "#haskell-lens" "#haskell-in-depth" "#haskell-beginners" "#xmonad"
;;            "#idris" "#categorytheory"
;;            ))
;;         erc-nickserv-passwords
;;         '((freenode     ((matt/irc-username . matt/freenode-pass)))))
;;   (erc :server "irc.freenode.net" :port 6667 :nick matt/irc-username :password matt/freenode-pass)

;;   (add-to-list 'erc-modules 'scrolltobottom)
;;   )

;;------------------------------------------------------------------------------
;; GNUS RSS

(use-package alert
  :commands (alert)
  :init
  (setq alert-default-style 'notifier))

(provide 'matt-utils)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; matt-utils.el ends here
