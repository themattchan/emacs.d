;;==============================================================================
;; Themes
;;==============================================================================
(provide 'matt-themes)

(defvar matt/themes '(monokai spacegray))
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/emacs-theme-darktooth/")

(if (not (window-system))
    (progn
      ;; default theme on terminals
      (load-theme 'monokai t)           ; wombat
      (set-background-color "black")))
