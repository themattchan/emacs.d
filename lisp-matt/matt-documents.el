;;; matt-documents.el --- Writing & document modes.

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
;; General text mode
;;------------------------------------------------------------------------------

(use-package pandoc-mode
  :init
  (add-hook 'pandoc-mode-hook 'pandoc-load-default-settings))

(use-package flyspell
  :diminish
  :hook (text-mode . flyspell-mode))

(use-package captain
  :diminish
  :hook (text-mode . global-captain-mode))

(add-hook 'text-mode-hook 'ac-ispell-ac-setup)
(add-hook 'text-mode-hook
          (lambda ()
            (visual-line-mode 1)
            (setq
             ;; tabs to spaces in text mode
             indent-tabs-mode nil
             ;; Default tabs in text is 4 spaces
             tab-width 4
             ;; default insert is also 4 and inc of 4
             ;; got to specify this or it will continue to expand to 8 spc
             tab-stop-list (number-sequence 4 120 4)
             )))

(use-package align
  :init
  (add-hook 'align-load-hook
            (lambda ()
              (setq align-text-modes (append '(fundamental-mode markdown-mode) align-text-modes)))))

;;------------------------------------------------------------------------------
;; LaTeX and AUCTeX
;;------------------------------------------------------------------------------
(use-package auctex
  :mode ("\\.tex\\'" . TeX-latex-mode))
(use-package reftex
  :after auctex
  :init
  (add-hook 'reftex-load-hook 'imenu-add-menubar-index)
  (add-hook 'reftex-mode-hook 'imenu-add-menubar-index))

(use-package latex
  :after reftex
  :init
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (setq TeX-auto-save t
                    TeX-parse-self t
                    TeX-auto-untabify t
                    TeX-master nil
                    TeX-PDF-mode t)))
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (add-hook 'LaTeX-mode-hook (setq reftex-plug-into-AUCTeX t))


  ;; Auto-complete mode
  ;; breaks dwim or some shit on Arch
  ;; (when *is-mac*  (eval-after-load "LaTeX-mode" (require 'auto-complete-auctex)))
  ;; (autoload 'ac-math "ac-math")
  ;; (defun ac-latex-mode-setup ()         ; add ac-sources to default ac-sources
  ;;   (setq ac-sources
  ;;      (append '(ac-source-math-unicode ac-source-math-latex ac-source-latex-commands)
  ;;                ac-sources)))
  ;; (add-hook 'latex-mode-hook 'ac-latex-mode-setup)
  ;; (ac-flyspell-workaround)

  (add-hook 'LaTeX-mode-hook (lambda ()
                               (push
                                '("Latexmk" "latexmk -pdf %(mode) %s" TeX-run-TeX nil t
                                  :help "Run Latexmk on file")
                                TeX-command-list)))


  (add-hook 'LaTeX-mode-hook 'company-auctex-init))
;; (setq LaTeX-section-hook
;;       '(LaTeX-section-heading
;;         LaTeX-section-title
;;         ;; LaTeX-section-toc
;;         LaTeX-section-section
;;         LaTeX-section-label))

;;------------------------------------------------------------------------------
;; Markdown
;;------------------------------------------------------------------------------

(use-package markdown-mode
  :mode ("\\.text\\'" "\\.markdown\\'" "\\.md\\'")
  :init
  ;;(add-hook 'markdown-mode-hook 'turn-on-pandoc)
  ;; markdown check paren balancing
  (add-hook 'markdown-mode-hook
               (lambda ()
                 (when buffer-file-name
                   (add-hook 'after-save-hook
                             'check-parens
                             nil t)))))

;;------------------------------------------------------------------------------
;; Org-mode
;;------------------------------------------------------------------------------

(use-package org
  :config
  (setq org-startup-truncated nil
        org-fontify-whole-heading-line t
        org-src-fontify-natively t)
  (add-to-list 'org-latex-packages-alist '("" "minted"))
  (setq org-latex-listings 'minted)
  (setq org-latex-pdf-process
        '("latexmk -pdflatex='xelatex --shell-escape' -pdf %f"))

  (add-hook 'org-mode-hook 'turn-on-stripe-table-mode)

  (add-hook 'org-mode-hook
            (lambda ()
              (set-face-attribute 'org-level-1 nil :height 120))))

(use-package ox-latex :after org)
(put 'upcase-region 'disabled nil)

;; (setq org-export-html-style-include-scripts nil
;;       org-export-html-style-include-default nil)


;; (defun get-string-from-file (filePath)
;;   "Return filePath's file content."
;;   (with-temp-buffer
;;     (insert-file-contents filePath)
;;     (buffer-string)))

;; (setq matt/org-export-default-css
;;       (format "<style>%s</style>" (get-string-from-file "~/.emacs.d/org-style.css")))

;; (setq org-export-html-style matt/org-export-default-css)

;; (defun matt/org-include-img-from-pdf (&rest ignore)
;;   "Convert the pdf files to image files.

;; Only looks at #HEADER: lines that have \":convertfrompdf t\".
;; This function does nothing if not in org-mode, so you can safely
;; add it to `before-save-hook'."
;;   (interactive)
;;   (when (derived-mode-p 'org-mode)
;;     (save-excursion
;;       (goto-char (point-min))
;;       (while (search-forward-regexp
;;               "^\\s-*#\\+HEADER:.*\\s-:convertfrompdf\\s-+t"
;;               nil 'noerror)
;;         (let* (filenoext imgext imgfile pdffile cmd)
;;           ;; Keep on going on to the next line till it finds a line with
;;           ;; `[[FILE]]'
;;           (while (progn
;;                    (forward-line 1)
;;                    (not (looking-at "\\[\\[\\(.*\\)\\.\\(.*\\)\\]\\]"))))
;;           (when (looking-at "\\[\\[\\(.*\\)\\.\\(.*\\)\\]\\]")
;;             (setq filenoext (match-string-no-properties 1))
;;             (setq imgext (match-string-no-properties 2))
;;             (setq imgfile (expand-file-name (concat filenoext "." imgext)))
;;             (setq pdffile (expand-file-name (concat filenoext "." "pdf")))
;;             (setq cmd (concat "convert -density 96 -quality 85 "
;;                               pdffile " " imgfile))
;;             (when (file-newer-than-file-p pdffile imgfile)
;;               ;; This block is executed only if pdffile is newer than imgfile
;;               ;; or if imgfile does not exist
;;               ;; Source: https://www.gnu.org/software/emacs/manual/html_node/elisp/Testing-Accessibility.html
;;               (message "%s" cmd)
;;               (shell-command cmd))))))))
;; ;(add-hook 'before-save-hook #'matt/org-include-img-from-pdf)
;; (add-hook 'org-export-before-processing-hook #'matt/org-include-img-from-pdf)

;; (defun matt/add-org-publish-config (config)
;;   "Register an org-project config"
;;   (setq org-publish-project-alist
;;         (cons config org-publish-project-alist)))

;; (defun matt/remove-org-publish-config (config-name)
;;   "Remove a config with config-name (string) from the config list"
;;   (setq org-publish-project-alist
;;         (cl-remove-if (lambda (config) (string= config-name (car config)))
;;                       org-publish-project-alist)))

;;(setq matt/file-loaded-set '())         ; PRIVATE -- things I've loaded
;; (setq matt/org-project-file-name "org-project.el")

;; (defmacro set-add! (var set)
;;   "Lists as Sets"
;;   `(progn
;;     (setq ,set (delq ,var ,set))
;;     (push ,var ,set)))

;; (defun matt/file-loaded-p (filename)
;;   "Has file been loaded yet?"
;;   (memq (intern filename) matt/file-loaded-set))

;; (defun matt/safe-load-file (filename &rest cl-keys)
;;   "Load file only if not loaded yet or if forced by kw-arg"
;;   (cl--parsing-keywords ((:force nil)) nil
;;     (when (or cl-force
;;               (not (matt/file-loaded-p filename)))
;;       (load-file filename)
;;       (set-add! (intern filename) matt/file-loaded-set))))

;; (defun matt/load-org-project-settings (&rest cl-keys)
;;   "Keep going up the tree looking for the settings file, then load it"
;;   (interactive)
;;   (let ((project-dir
;;          (locate-dominating-file
;;           (file-name-directory (or load-file-name buffer-file-name))
;;           matt/org-project-file-name))
;;         (force (or (called-interactively-p 'any)
;;                     (cl--parsing-keywords ((:force nil)) nil
;;                       cl-force))))
;;     (when project-dir
;;       (let ((filename (concat project-dir matt/org-project-file-name)))
;;         (matt/safe-load-file filename :force force)))))

;; (add-hook 'org-mode-hook 'matt/load-org-project-settings)

;; graphviz
(add-to-list 'auto-mode-alist '("\\.gv\\'" . graphviz-dot-mode))
(add-to-list 'auto-mode-alist '("\\.dot\\'" . graphviz-dot-mode))

;; open info files in the interactive browser
(add-to-list 'auto-mode-alist '("\\.info\\'" . info-mode))

(add-to-list 'auto-mode-alist '("\\.ott\\'" . ott-mode))

(provide 'matt-documents)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;; matt-documents.el ends here
