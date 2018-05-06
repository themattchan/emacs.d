;;; matt-themes.el --- Load various themes.

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

(setq lexical-binding t)

(if (window-system)
    (load-theme 'badwolf t) ; odersky
  (progn
    ;; default theme on terminals
    (load-theme 'wombat t)
    (set-background-color "black")))


;; TODO rewrite this using frame configurations
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Frames.html#Frames
(eval-and-compile
(defconst DEFAULT-FONT-SIZE (face-attribute 'default :height))

(defun small-fonts ()
  (interactive)
  (set-face-attribute 'default nil :height (truncate (* DEFAULT-FONT-SIZE 0.75))))

(defun normal-fonts ()
  (interactive)
  (set-face-attribute  'default nil :height DEFAULT-FONT-SIZE))

(defun maximize-frame ()
 "Maximize the frame
(this is cribbed from the definition of toggle-frame-maximized)"
  (interactive)
  (let ((fullscreen (frame-parameter nil 'fullscreen)))
    (when (not (eq fullscreen 'maximized))
      (set-frame-parameter nil 'fullscreen 'maximized))))

(defun layout-4column ()
  (interactive)
  (small-fonts)
  (delete-other-windows)
  (dotimes (_ 3) (split-window-right))
  (balance-windows)
  (maximize-frame))

(defun layout-2by4 ()
  (interactive)
  (small-fonts)
  (delete-other-windows)
  (split-window-below)
  (dotimes (_ 3) (split-window-right))
  (other-window -1)
  (dotimes (_ 3) (split-window-right))
  (balance-windows)
  (maximize-frame))
);; eval-and-compile

(provide 'matt-themes)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; matt-themes.el ends here
