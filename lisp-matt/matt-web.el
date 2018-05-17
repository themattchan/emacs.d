;;; matt-web.el --- Settings for editing web stuff.

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


(use-package web-mode
  :defer t
  :mode
  (("\\.html$" . web-mode)
   ("\\.scala.html\\'" . web-mode)
   ("\\.css\\'" . web-mode)
   ("\\.scss\\'" . web-mode))
  :config
  (add-hook 'web-mode-hook
            (lambda ()
              (setq indent-tabs-mode nil
                    tab-width 2
                    web-mode-variable-offset 2
                    web-mode-html-offset 2
                    web-mode-markup-indent-offset 2
                    web-mode-css-indent-offset 2
                    web-mode-code-indent-offset 2
                    web-mode-engines-alist '(("razor" . "\\.scala.html\\'")
                                             ("blade" . "\\.blade\\."))))))

(use-package js2-mode
  :defer t
  :mode (("\\.js\\'" . js2-mode))
  :config
  (setq indent-tabs-mode nil)
  (setq js-indent-level 2)
  (setq js2-bounce-indent-p t)
  (setq js2-auto-indent-p nil)
  (setq js2-basic-offset 2)
  (add-hook 'js2-mode-hook #'js2-refactor-mode))

(use-package js2-refactor
  :defer t
  :after js2-mode)

(use-package json-mode
  :defer t
  :mode (("\\.json\\'" . json-mode))
  :config
  (setq json-reformat:indent-width 2))

(use-package rjsx-mode
  :defer t
  :mode (("\\.jsx$" . rjsx-mode)))

(provide 'matt-web)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; matt-web.el ends here
