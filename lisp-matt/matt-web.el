;;; matt-web.el --- Settings for editing web stuff.

;;; Copyright (c) 2013-2015 Matthew Chan
;;; Author: Matthew Chan <matt@parametri.city>
;;; URL: http://github.com/themattchan/emacs.d

;;; Commentary:

;;; Code:


(autoload 'web-mode "web-mode" "" t)
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))


(add-to-list 'auto-mode-alist '("\\.scala.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . web-mode))

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
                                           ("blade" . "\\.blade\\.")))))

(add-to-list 'auto-mode-alist '("\\.js\\'" . js3-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . js3-mode))

(setq js2-bounce-indent-p t)
(setq js2-auto-indent-p nil)
(setq js2-basic-offset 2)

(provide 'matt-web)
;;; matt-web.el ends here
