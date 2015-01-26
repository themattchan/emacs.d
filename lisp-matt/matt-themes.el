;;==============================================================================
;; Themes
;;==============================================================================


;; set the theme depending on buffer.
;; requires the load-theme-buffer-local package
;; (when (>= emacs-major-version 24)
;;   ;; Cycle through this set of themes
;;   (cond ((window-system)
;; 		 (progn (setq my-themes '(spacegray tango-dark 'nil))
;; 				(setq my-cur-theme nil)))
;; 		((not window-system)
;; 		 (load-theme 'tango-dark t)))

;;   (defun cycle-my-theme ()
;; 	"Cycle through a list of themes, my-themes"
;; 	(interactive)
;; 	(when my-cur-theme
;; 	  (disable-theme my-cur-theme)
;; 	  (setq my-themes (append my-themes (list my-cur-theme))))
;; 	(setq my-cur-theme (pop my-themes))
;; 	(load-theme t my-cur-theme))

;;   ;; Switch to the first theme in the list above
;;   (when (window-system)
;; 	(cycle-my-theme))

;;   ;; Bind this to C-t
;;   (global-set-key (kbd "C-t") 'cycle-my-theme)
;;   (defun on-after-init ()
;; 	(unless (display-graphic-p (selected-frame))
;; 	  (set-face-background 'default "unspecified-bg" (selected-frame))))

;;   (add-hook 'window-setup-hook 'on-after-init)
;;   )

;; (cond
;;  ((and (>= emacs-major-version 24) (window-system))
;; 		(progn
;; 		  (add-hook 'prog-mode-hook (lambda () (enable-theme-buffer-local 'tango-dark (current-buffer))))
;; 		  (add-hook 'text-mode-hook (lambda () (enable-theme-buffer-local 'dichromacy (current-buffer))))
;; 		  ))
;; 	  ((and (>= emacs-major-version 24)(not window-system))
;; 		(load-theme 'tango-dark t)
;; 	  ))



;; (defun cycle-theme
;;   "Cycle through a list of themes I like"
;;   )
;;'(spacegray dichromacy marquardt)

(if (not (window-system))
	(progn
	  ;; default theme on terminals
	  (load-theme 'wombat t)
	  (set-background-color "black")))

(provide 'matt-themes)
