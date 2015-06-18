;;==============================================================================
;; Themes
;;==============================================================================
(provide 'matt-themes)

(defvar matt/themes '(monokai spacegray))

(if (not (window-system))
    (progn
      ;; default theme on terminals
      (load-theme 'monokai t)           ; wombat
      (set-background-color "black")))
