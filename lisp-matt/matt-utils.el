;;; matt-utils.el --- Settings for various "app modes"

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
;; Dired
;; hide the -al stuff, toggle with ( and )
(require 'dired-details+)
(setq dired-recursive-deletes 'always
      dired-recursive-copies 'always
      delete-by-moving-to-trash t
      trash-directory "~/.Trash/"
      ;; show sym link targets
      Dired-details-hide-link-targets nil)
(add-hook 'dired-mode-hook 'turn-on-stripe-buffer-mode)
;; (add-hook 'dired-mode-hook 'stripe-listify-buffer)

(require 'dired-x)
(setq-default ; M-o to toggle omit mode
 dired-omit-mode t
 dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\.")

;;------------------------------------------------------------------------------
;; Terminal
;; (use-package multi-term
;;   :ensure t
;;   :commands (multi-term)
;;   :bind (("M-o M-t" . multi-term)
;;          ("M-[" . multi-term-prev)
;;          ("M-]" . multi-term-next))
;;   :init
;;   (progn
;;     (setq multi-term-program "/bin/zsh")))
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(setenv "ESHELL" "bash")

;;------------------------------------------------------------------------------
;; TRAMP-mode
(setq tramp-default-method "ssh")
(add-to-list 'tramp-default-method-alist '("ieng9.ucsd.edu" "" "scpx"))
(eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))
(setq password-cache-expiry nil)

;;------------------------------------------------------------------------------
;; IRC
;; mostly from https://github.com/bryangarza/dot-emacs/blob/master/init.el#L1273
(setq erc-server-coding-system '(utf-8 . utf-8))
(setq erc-timestamp-format "[%I:%M %p]")
(setq erc-hide-timestamps t)
(setq erc-echo-timestamps nil)
(setq erc-echo-timestamp-format "TS'd %A, %I:%M:%S %p")
(setq erc-track-showcount t)
(setq erc-track-enable-keybindings t)
(add-to-list 'erc-modules 'scrolltobottom)

;(setq erc-hide-list '("JOIN" "PART" "QUIT"))
(setq erc-hide-list '("MODE" "324" "329" "332" "333" "353"))
(setq erc-lurker-hide-list '("JOIN" "PART" "QUIT"))



(provide 'matt-utils)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; matt-utils.el ends here
