(provide 'matt-prog-other)

;; Scala
;; scala2-mode handles tabs to 2 spaces by default
(add-to-list 'auto-mode-alist '("\.scala" . scala-mode) '("\.sbt\'" . scala-mode))

;; Python
(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))
(add-hook 'python-mode-hook
          (lambda()
            (setq-default indent-tabs-mode nil
                          tab-width 4
                          ;;tab-stop-list (number-sequence 2 120 2)
                          py-indent-offset 4
                          ;;python-indent-offset 2
                          )))

;; javascript
(add-to-list 'auto-mode-alist '("\.js" . js2-mode))

;; a sane html mode that indents embedded crap properly
(autoload 'web-mode "web-mode" "" t)
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))

;; THIS IS GARBAGE
;; multi-web-mode (html + js + css)
;; (autoload 'multi-web-mode "multi-web-mode" "" t)
;; (eval-after-load "multi-web-mode"
;;   (lambda ()
;;     (progn
;;      (setq mweb-default-major-mode 'html-mode)
;;      (setq mweb-tags
;;            '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
;;              (js2-mode  "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
;;              (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
;;      (setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
;;      (multi-web-global-mode 1))))
