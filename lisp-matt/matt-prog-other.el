;;; matt-prog-other.el --- Settings for miscellaneous programming languages.

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


;; Scala
;; scala2-mode handles tabs to 2 spaces by default
(add-to-list 'auto-mode-alist '("\.scala" . scala-mode) '("\.sbt\'" . scala-mode))

(add-hook 'scala-mode-hook
          (lambda ()
             (local-set-key (kbd "RET")
                 '(lambda ()
                    (interactive)
                    (newline-and-indent)
                    (scala-indent:insert-asterisk-on-multiline-comment)))
))

;; Python
(use-package python-mode
  :mode ("\\.py\\'" . python-mode)
  :config
  (add-hook 'python-mode-hook
            (lambda()
              (elpy-enable)
              (setq-default indent-tabs-mode nil
                            tab-width 4
                            ;;tab-stop-list (number-sequence 2 120 2)
                            ;;py-indent-offset 4
                            ;;python-indent-offset 2
                            ))))

;; Ampl mode

(use-package ampl-mode
  :mode (("\\.mod$" . ampl-mode)
         ("\\.dat$" . ampl-mode)
         ("\\.ampl$" . ampl-mode))
  :config
  (setq interpreter-mode-alist
      (cons '("ampl" . ampl-mode)
            interpreter-mode-alist)))

;; CSE 131
;; (load-file "~/Dropbox/cse131/cse131.el/cse131.el")
;; (require 'cse131)
;; (add-to-list 'auto-mode-alist
;;              `(,(regexp-opt '(".adder" ".boa" ".cobra" ".diamond" ".egg" ".fdl")) . cse131-mode))

(provide 'matt-prog-other)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; matt-prog-other.el ends here
