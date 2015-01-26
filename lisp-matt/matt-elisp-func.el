;;==============================================================================
;; FUNCTIONS
;;==============================================================================
(defun matt/recompile-settings ()
  (interactive)
  (byte-recompile-file "~/.emacs.d/init.el" 0)
  (byte-recompile-file "~/.emacs.d/custom-24.el" 0)
  (byte-recompile-directory "~/.emacs.d/lisp-matt" 0)
  (load-file "~/.emacs.d/init.elc"))

;; only one theme at a time
(defadvice load-theme
  (before theme-dont-propagate activate)
  (mapc #'disable-theme custom-enabled-themes))

(defun matt/load-theme (theme)
  (if (window-system)
    (load-theme theme)))

;; expand filled paragraph to a line
(defun unfill-paragraph ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

;; Yuck CRLF.
;; Function for converting from DOS to UNIX line ends
(defun unix-file ()
  "Change the current buffer to Latin 1 with Unix line-ends."
  (interactive)
  (set-buffer-file-coding-system 'utf-8-unix t))
(defun dos-file ()
  "Change the current buffer to Latin 1 with DOS line-ends."
  (interactive)
  (set-buffer-file-coding-system 'utf-8-dos t))
(defun mac-file ()
  "Change the current buffer to Latin 1 with Mac line-ends."
  (interactive)
  (set-buffer-file-coding-system 'utf-8-mac t))

;; auto create nonexistent dirs
(defun my-create-non-existent-directory ()
      (let ((parent-directory (file-name-directory buffer-file-name)))
        (when (and (not (file-exists-p parent-directory))
                   (y-or-n-p (format "Directory `%s' does not exist! Create it?" parent-directory)))
          (make-directory parent-directory t))))
(add-to-list 'find-file-not-found-functions #'my-create-non-existent-directory)

;; M-x google!
(defun google ()
  "Google the selected region if any, display a query prompt otherwise."
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (url-hexify-string (if mark-active
                           (buffer-substring (region-beginning) (region-end))
                         (read-string "Google: "))))))

;; open the current file in another program. from emacs prelude.
(defun open-with (arg)
  "Open visited file in default external program.

With a prefix ARG always prompt for command to use."
  (interactive "P")
  (when buffer-file-name
    (shell-command (concat
                    (cond
                     ((and (not arg) *is-a-mac*) "open")
                     ((and (not arg) *is-linux*) "xdg-open")
                     (t (read-shell-command "Open current file with: ")))
                    " "
                    (shell-quote-argument buffer-file-name)))))

(provide 'matt-elisp-func)
