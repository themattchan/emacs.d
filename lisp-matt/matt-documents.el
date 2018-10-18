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

(add-hook 'text-mode-hook
          (lambda ()
            (visual-line-mode 1)
            (auto-fill-mode)
            (setq
             ;; tabs to spaces in text mode
             indent-tabs-mode nil
             ;; Default tabs in text is 4 spaces
             tab-width 4
             ;; default insert is also 4 and inc of 4
             ;; got to specify this or it will continue to expand to 8 spc
             tab-stop-list (number-sequence 4 120 4)
             )))

(add-hook 'align-load-hook
          (lambda ()
            (setq align-text-modes (append '(fundamental-mode markdown-mode) align-text-modes))))
;; (use-package align
;;   :init
;;   )

;;------------------------------------------------------------------------------
;; LaTeX and AUCTeX
;;------------------------------------------------------------------------------
(use-package auctex
  :defer t
  :mode ("\\.tex\\'" . TeX-latex-mode))
(use-package reftex
  :after auctex
  :defer t
  :init
  (add-hook 'reftex-load-hook 'imenu-add-menubar-index)
  (add-hook 'reftex-mode-hook 'imenu-add-menubar-index))

(use-package latex
  :after reftex
  :defer t
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
  :defer t
  :init
  (add-hook 'markdown-mode-hook 'turn-on-pandoc)
  ;; markdown check paren balancing
  ;; (add-hook 'markdown-mode-hook
  ;;              (lambda ()
  ;;                (when buffer-file-name
  ;;                  (add-hook 'after-save-hook
  ;;                            'check-parens
  ;;                            nil t))))
  )

;;------------------------------------------------------------------------------
;; Org-mode
;;------------------------------------------------------------------------------

;; SEE: https://github.com/gjstein/emacs.d/blob/master/config/gs-org.el
(use-package org
  :defer t
  :init (use-package s)
  :config
  (use-package org-agenda)
  (use-package org-capture)

  (use-package org-journal
    :config
    (setq org-journal-dir "~/Dropbox/writing/diary"
          org-journal-file-format "%Y-%m-%d.org"
          org-journal-date-format "%A, %d %B %Y"))

  (use-package org-wiki
    :config
    (setq org-wiki-location "~/Dropbox/notes/wiki"
          org-wiki-close-root-switch t
          org-wiki-server-host "127.0.0.1"))

  (setq org-startup-truncated nil
        org-fontify-whole-heading-line t
        org-src-fontify-natively t)

  ;; latex export
  (add-to-list 'org-latex-packages-alist '("" "minted"))
  (setq org-latex-listings 'minted)
  (setq org-latex-pdf-process
        '("latexmk -pdflatex='xelatex --shell-escape' -pdf %f"))

  (add-hook 'org-mode-hook 'turn-on-stripe-table-mode)

  (setq org-modules '(org-gnus
                      org-drill
                      org-habit
                      org-annotate-file
                      org-toc))

  ;; GTD
  (let ((matt/TODO-INBOX "~/Dropbox/todo/TODO.org")
        (matt/TODO-SOMEDAY "~/Dropbox/todo/someday.org")
        (matt/TODO-PROJECTS "~/Dropbox/todo/projects.org")
        (matt/TODO-WORK "~/Dropbox/todo/awake.org")
        (matt/LEARN "~/Dropbox/todo/learn.org")
        (matt/org-default-timestamp-fmt "[%<%Y-%m-%d %b %H:%M>]"))

    (global-set-key "\C-cl" 'org-store-link)
    (global-set-key "\C-ca" 'org-agenda)
    (global-set-key "\C-cc" 'org-capture)
    (global-set-key "\C-ct" '(lambda () (interactive) (org-capture :keys "t")))
    (global-set-key "\C-cb" 'org-switchb)

    (setq org-log-done 'time) ;; log the time whe things are done

    (setq org-capture-templates
          ;; https://orgmode.org/manual/Template-expansion.html#Template-expansion
          `(("t" "TODO [inbox]" entry
             (file ,matt/TODO-INBOX)
             ,(s-concat "* TODO %i%?\n " matt/org-default-timestamp-fmt))

            ("T" "someday" entry
             (file ,matt/TODO-SOMEDAY)
             ,(s-concat "* TODO %i%? \n " matt/org-default-timestamp-fmt))

            ("w" "work" entry
             (file ,matt/TODO-WORK)
             ,(s-concat "* TODO %i%?\n " matt/org-default-timestamp-fmt))
            ))

    ;; http://doc.endlessparentheses.com/Var/org-refile-targets.html
    ;; :level N --- only headlines at level N
    ;; :maxlevel N --- headlines from levels 1 to N
    (setq org-refile-targets `((,matt/TODO-SOMEDAY :level . 1)
                               (,matt/LEARN :level . 2)
                               (,matt/TODO-PROJECTS :maxlevel . 3)
                               (,matt/TODO-WORK :maxlevel . 3)
                               ))

    (setq org-agenda-files (list matt/TODO-INBOX matt/TODO-PROJECTS matt/TODO-SOMEDAY matt/TODO-WORK))

    ;; NICETO: have the bullets (but NOT the text) display as the utf8 glyphs
    ;; when the keyword is specified
;;    (setq org-todo-keywords '((sequence "☛ TODO(t)" "⚑ WAITING(w)" "|" "✔ DONE(d)" "✘ CANCELLED(c)")))
    (setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))

    (setq org-tag-alist '(("@computer" . ?c)
                          ("@phone" . ?p)
                          ("errand" . ?e)
                          ))


  ;; see the M-x customize screen for options
  ;; EXAMPLE
  ;; (setq org-agenda-custom-commands
  ;;     '(("x" agenda)
  ;;       ("y" agenda*)
  ;;       ("w" todo "WAITING")
  ;;       ("W" todo-tree "WAITING")
  ;;       ("u" tags "+boss-urgent")
  ;;       ("v" tags-todo "+boss-urgent")
  ;;       ("U" tags-tree "+boss-urgent")
  ;;       ("f" occur-tree "\\<FIXME\\>")
  ;;       ("h" . "HOME+Name tags searches") ; description for "h" prefix
  ;;       ("hl" tags "+home+Lisa")
  ;;       ("hp" tags "+home+Peter")
  ;;       ("hk" tags "+home+Kim")))
    (setq org-agenda-custom-commands
          '(("x" "Agenda"
             ((agenda "" ((org-agenda-overriding-header "Today's Schedule:")
					                (org-agenda-span 'day)
					                (org-agenda-ndays 1)
					                (org-agenda-start-on-weekday nil)
					                (org-agenda-start-day "+0d")
                          (org-agenda-todo-ignore-deadlines nil)))
               ;; TODO tweak this
               (tags-todo "@computer")
               (tags-todo "@phone")
               (tags-todo "errand")
             ))

            ("c" "computer tasks" tags-todo "@computer")
            ("p" "phone calls" tags-todo "@phone")
            ("e" "errands" tags-todo "errand")
            ))

    ) ;; let

  (org-load-modules-maybe t) ;; load modules
  )

(use-package ox-latex :after org)
(put 'upcase-region 'disabled nil)

;; graphviz
(use-package graphviz-dot-mode
  :defer t
  :mode
  (("\\.gv\\'" . graphviz-dot-mode)
   ("\\.dot\\'" . graphviz-dot-mode)))

;; open info files in the interactive browser
(add-to-list 'auto-mode-alist '("\\.info\\'" . info-mode))

(add-to-list 'auto-mode-alist '("\\.ott\\'" . ott-mode))

(provide 'matt-documents)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;; matt-documents.el ends here
