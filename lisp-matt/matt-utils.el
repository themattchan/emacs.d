;;; matt-utils.el --- Settings for system utility replacements.

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

;; dired
;; hide the -al stuff, toggle with ( and )
(require 'dired-details+)
(setq dired-recursive-deletes 'always
      dired-recursive-copies 'always
      delete-by-moving-to-trash t
      trash-directory "~/.Trash/"
      ;; show sym link targets
      Dired-details-hide-link-targets nil)
(require 'dired-x)
(setq-default ; M-o to toggle omit mode
 dired-omit-mode t
 dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\.")

;; terminal
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

;; tramp mode
(setq tramp-default-method "ssh")
(eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))

(provide 'matt-utils)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; matt-utils.el ends here
