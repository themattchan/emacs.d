;;; matt-prog-functional.el --- Global settings for functional programming languages.

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
(eval-when-compile (require 'use-package))

;; Proof General and Coq
(use-package proof-site
  :defer t
  :load-path (lambda () (concat user-emacs-directory "lisp/PG/generic"))
  :mode ("\\.v\\'" . coq-mode)
  :init (setq proof-splash-enable nil))

(use-package coq
  :defer t
  :after proof-site
  :load-path (lambda () (concat user-emacs-directory "lisp/PG/coq"))
  :config
  (if *is-mac* (setq coq-prog-name "/usr/local/bin/coqtop"))

  (setq coq-default-undo-limit 10000)
  ;;    (setq proof-general-directory "~/.emacs.d/lisp/PG")
  )

(use-package company-coq
  :commands (company-coq-mode)
  :init (add-hook 'coq-mode-hook 'company-coq-mode t)
  :defer t
  :after (proof-site coq)
  :config
  (setq company-coq-disabled-features '(prettify-symbols)))

;;  Purescript

(use-package purescript-mode
  :defer t
  :config
    (use-package psc-ide
      :bind (:map psc-ide-mode-map
                  ("M-." . find-tag)))
    (psc-ide-mode)
    (company-mode)
    (flycheck-mode)
    (lsp-purescript-enable)
    (purescript-indentation-mode))

(eval-and-compile
  (defun purescript-make-tags ()
    (interactive)
    (projectile-with-default-dir (projectile-project-root)
      (shell-command "purs docs --format etags \"*.purs\" \"src/**/*.purs\" \"bower_components/*/src/**/*.purs\" >! TAGS"))
    (load-project-tags)))

(setq idris-interpreter-path "/usr/local/bin/idris")

(provide 'matt-prog-functional)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; matt-prog-functional.el ends here
