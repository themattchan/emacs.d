;;; matt-buffer-clean.el --- Buffer saving and cleanup.

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

;; (desktop-save-mode 1) ;;save & autoload buffers
;; (setq desktop-dirname             "~/.emacs.d/desktop/"
;;       desktop-path                '("~/.emacs.d/desktop")
;;       desktop-base-file-name      "emacs-desktop"
;;       desktop-base-lock-name      "lock"
;;       desktop-save                nil
;;       desktop-files-not-to-save   "^$" ;reload tramp paths
;;       desktop-load-locked-desktop nil)
;; (setq desktop-buffers-not-to-save
;;       (concat "\\("
;;               "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
;;               "\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb"
;;               "\\)$"))
;; (add-to-list 'desktop-modes-not-to-save 'dired-mode)
;; (add-to-list 'desktop-modes-not-to-save 'Info-mode)
;; (add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
;; (add-to-list 'desktop-modes-not-to-save 'fundamental-mode)

(use-package midnight)

(eval-after-load "midnight"
  '(progn
     (setq
      ;; expiration date of all buffers is 1 day
      clean-buffer-list-delay-general 1
      ;;  expiration date of special buffers is 1 hour
      clean-buffer-list-delay-special (* 1 60 60)
      )

     (defvar clean-buffer-list-timer nil
       "Stores clean-buffer-list timer if there is one.
You can disable clean-buffer-list by (cancel-timer clean-buffer-list-timer).")

     ;; run clean-buffer-list every 2 hours
     (setq clean-buffer-list-timer (run-at-time t (* 3 60 60) 'clean-buffer-list))

     ;; kill everything, clean-buffer-list is very intelligent at not killing
     ;; unsaved buffers.
     (setq clean-buffer-list-kill-regexps
           '("^.*$"
             "\\`\\*Customize .*\\*\\'"
             "\\`\\*\\(Wo\\)?Man .*\\*\\'"))
     ;; ;; special buffers to be killed every 6 hours
     (add-to-list 'clean-buffer-list-kill-buffer-names
                  '("*bffer-selection*"
                    "*Finder*"
                    "*Finder Category*"
                    "*Finder-package*"
                    "*RE-Builder*"
                    "*vc-change-log*"))
     ))

(provide 'matt-buffer-clean)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; matt-buffer-clean.el ends here
