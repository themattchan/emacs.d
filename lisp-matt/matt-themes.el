;;; matt-themes.el --- Load various themes.

;;; Copyright (c) 2013-2015 Matthew Chan
;;; Author: Matthew Chan <matt@parametri.city>
;;; URL: http://github.com/themattchan/emacs.d

;;; Commentary:

;;; Code:

(defvar matt/themes '(monokai spacegray))
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/emacs-theme-darktooth/")

(if (not (window-system))
    (progn
      ;; default theme on terminals
      (load-theme 'monokai t)           ; wombat
      (set-background-color "black")))

(provide 'matt-themes)
;;; matt-themes.el ends here
