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

(autoload 'pandoc-mode "pandoc-mode")
(add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)
(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)

(add-hook 'text-mode-hook 'ac-ispell-ac-setup)
(add-hook 'text-mode-hook
          (lambda ()
            (linum-mode 0)
            (visual-line-mode 1)
            (flyspell-mode 1)
            ;;(pandoc-mode 1)
            ;(turn-on-pandoc)
            (setq
             ;; tabs to spaces in text mode
             indent-tabs-mode nil
             ;; Default tabs in text is 4 spaces
             tab-width 4
             ;; default insert is also 4 and inc of 4
             ;; got to specify this or it will continue to expand to 8 spc
             tab-stop-list (number-sequence 4 120 4)
             )

             (turn-on-auto-fill))
            ;; ask to turn on hard line wrapping
            ;; (if (y-or-n-p "Hard wrap text?")
            ;;   (progn
            ;;     (fci-mode 1)
            ;;     (turn-on-auto-fill))
            ;;   (progn
            ;;     (fci-mode 0)))
          )

;; defadvice auto-fill-mode fci-mode

;;------------------------------------------------------------------------------
;; LaTeX and AUCTeX
;;------------------------------------------------------------------------------

(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

(add-hook 'LaTeX-mode-hook
          (lambda ()
            (setq TeX-auto-save t
                  TeX-parse-self t
                  TeX-auto-untabify t
                  TeX-master nil
                  TeX-PDF-mode t)))

(add-hook 'reftex-load-hook 'imenu-add-menubar-index)
(add-hook 'reftex-mode-hook 'imenu-add-menubar-index)
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


(add-hook 'LaTeX-mode-hook 'company-auctex-init)
;; (setq LaTeX-section-hook
;;       '(LaTeX-section-heading
;;         LaTeX-section-title
;;         ;; LaTeX-section-toc
;;         LaTeX-section-section
;;         LaTeX-section-label))

;;------------------------------------------------------------------------------
;; Markdown
;;------------------------------------------------------------------------------

(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(eval-after-load "markdown-mode"
  (progn
     ;; markdown check paren balancing
     (add-hook 'markdown-mode-hook
               (lambda ()
                 (when buffer-file-name
                   (add-hook 'after-save-hook
                             'check-parens
                             nil t))))))

;(add-hook 'markdown-mode-hook 'turn-on-pandoc)

;;------------------------------------------------------------------------------
;; Org-mode
;;------------------------------------------------------------------------------

(setq org-startup-truncated nil
      org-fontify-whole-heading-line t
      org-src-fontify-natively t)

(require 'ox-latex)
(add-to-list 'org-latex-packages-alist '("" "minted"))
(setq org-latex-listings 'minted)
(setq org-latex-pdf-process
      '("latexmk -pdflatex='xelatex --shell-escape' -pdf %f"))

(add-hook 'org-mode-hook
          (lambda ()
            (set-face-attribute 'org-level-1 nil :height 120)))
(put 'upcase-region 'disabled nil)

;; graphviz
(add-to-list 'auto-mode-alist '("\\.gv\\'" . graphviz-dot-mode))
(add-to-list 'auto-mode-alist '("\\.dot\\'" . graphviz-dot-mode))

;; open info files in the interactive browser
(add-to-list 'auto-mode-alist '("\\.info\\'" . info-mode))

(provide 'matt-documents)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;; matt-documents.el ends here
