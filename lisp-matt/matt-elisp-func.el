;;; matt-elisp-func.el --- Emacs Lisp functions.

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
(eval-and-compile
  (defmacro diminish-minor-mode (mode str)
    `(with-eval-after-load 'diminish
       (diminish ,mode ,str)))

  ;; (defvar diminish-major-mode-alist nil)

  ;;   (defmacro diminish-major-mode (mode str)
  ;;     `(push (,mode . ,str) diminish-major-mode-alist))

  ;;   (add-hook 'after-change-major-mode-hook
  ;;    (lambda ()
  ;;     (let ((s (alist-get major-mode diminish-major-mode-alist)))
  ;;       (when s (setq mode-name s)))))
  (defmacro diminish-major-mode (mode str)
                                        ;    `(with-eval-after-load ,mode
    `(add-hook 'after-change-major-mode-hook
               (lambda ()
                 (when (eq major-mode ,mode)
                   (setq mode-name ,str)))))
  )

(eval-and-compile
  (eval-when-compile (require 'cl))

  ;; expand filled paragraph to a line
  (defun unfill-paragraph ()
    (interactive)
    (let ((fill-column (point-max)))
      (fill-paragraph nil)))

  (defun sort-strings (xs)
    (sort xs'string-lessp))

  (defun selected-or-symbol-at-point ()
    (if (use-region-p)
        (buffer-substring-no-properties (region-beginning) (region-end))
      (thing-at-point 'symbol)))

  (defun advice-unadvice (sym)
    "Remove all advices from symbol SYM."
    (interactive "aFunction symbol: ")
    (advice-mapc (lambda (advice _props) (advice-remove sym advice)) sym))

  ;;------------------------------------------------------------------------------
  ;; uniquify lines
  ;; http://emacswiki.org/emacs/DuplicateLines
  (defun uniquify-region-lines (beg end)
    "Remove duplicate adjacent lines in region."
    (interactive "*r")
    (save-excursion
      (goto-char beg)
      (while (re-search-forward "^\\(.*\n\\)\\1+" end t)
        (replace-match "\\1"))))

  (defun uniquify-buffer-lines ()
    "Remove duplicate adjacent lines in the current buffer."
    (interactive)
    (uniquify-region-lines (point-min) (point-max)))


  (defun backward-delete-whitespace-to-column ()
    "delete back to the previous column of whitespace, or as much whitespace as possible,
or just one char if that's not possible"
    (interactive)
    (if indent-tabs-mode
        (call-interactively 'backward-delete-char-untabify)
      (let ((movement (% (current-column) tab-width))
            (p (point)))
        (when (= movement 0) (setq movement tab-width))
        (save-match-data
          (if (string-match "\\w*\\(\\s-+\\)$" (buffer-substring-no-properties (- p movement) p))
              (backward-delete-char-untabify (- (match-end 1) (match-beginning 1)))
            (call-interactively 'backward-delete-char-untabify))))))

  (defun sort-lines-nocase ()
    (interactive)
    (let ((sort-fold-case t))
      (call-interactively 'sort-lines)))

  (defun kill-all-buffers ()
    (interactive)
    (mapc 'kill-buffer (buffer-list)))

  (defun kill-all-except-current-buffer ()
    (interactive)
    (mapc 'kill-buffer (cdr (buffer-list (current-buffer)))))

  (defun indent-marked-files ()
    (interactive)
    (dolist (file (dired-get-marked-files))
      (find-file file)
      (indent-region (point-min) (point-max))
      (save-buffer)
      (kill-buffer nil)))

  (defun do-files (files)
    (lambda (fun)
      (dolist (file files) ; path to file
        (unwind-protect
            (progn
              (find-file file)
              (funcall fun)
              (save-buffer))
          (kill-buffer nil)))))

  (defun indent-file ()
    (indent-region (point-min) (point-max)))

  (defun tabify-file ()
    (tabify (point-min) (point-max)))

  (defun do-marked-files (fun)
    ((do-files (dired-get-marked-files))
     fun))

  (defun tabify-marked-files ()
    (interactive)
    (do-marked-files #'tabify-file))


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

  (defun matt/get-package-activated-list ()
    `(defvar matt/packages
       ,package-activated-list
       "Default packages"
       ))

  (defun matt/save-package-list ()
    (let* ((oldbuf (current-buffer))
           (bufname (format "%s" (gensym)))
           (buf (generate-new-buffer bufname)))
      (print
       `(defvar matt/packages
          (quote ,package-activated-list)
          "Default packages"
          )
       buf)

      (print
       '(defun matt/packages-installed-p ()
          (loop for pkg in matt/packages
                when (not (package-installed-p pkg))
                do (return nil)
                finally (return t)))
       buf)

      (print '(provide 'matt-packages) buf)

      (set-buffer buf)
      (write-file "./matt-packages.el" t)
      (kill-buffer buf)

      (set-buffer oldbuf)))

  ;; (defvar hexcolour-keywords
  ;;   '(("#[abcdef[:digit:]]\\{6\\}"
  ;;      (0 (put-text-property (match-beginning 0)
  ;;                            (match-end 0)
	;; 		                       'face (list :background
	;; 			                                 (match-string-no-properties 0)))))))

  ;; (defun hexcolour-add-to-font-lock ()
  ;;   (interactive)
  ;;   (font-lock-add-keywords nil hexcolour-keywords))
(defun mu-open-in-external-app ()
  "Open the file where point is or the marked files in Dired in external
app. The app is chosen from your OS's preference."
  (interactive)
  (let* ((file-list
          (dired-get-marked-files)))
    (mapc
     (lambda (file-path)
       (let ((process-connection-type nil))
         (start-process "" nil "xdg-open" file-path))) file-list)))
  );; eval-and-compile

(provide 'matt-elisp-func)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; matt-elisp-func.el ends here
