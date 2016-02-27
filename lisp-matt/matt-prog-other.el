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
(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))
(add-hook 'python-mode-hook
          (lambda()
            ;; Set ipython as the python interpreter
            (setq
             python-shell-interpreter "ipython"
             python-shell-interpreter-args "--colors NoColor"
             python-shell-prompt-regexp "In \\[[0-9]+\\]: "
             python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: ")
             ;; python-shell-completion-setup-code "from IPython.core.completerlib import module_completion"
             ;; python-shell-completion-module-string-code "';'.join(module_completion('''%s'''))\n"
             ;; python-shell-completion-string-code "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

            (setq-default indent-tabs-mode nil
                          tab-width 4
                          ;;tab-stop-list (number-sequence 2 120 2)
                          ;;py-indent-offset 4
                          ;;python-indent-offset 2
                          )))

;; (with-eval-after-load 'flycheck
;;     (flycheck-define-checker python-pep257
;;                              "A Python syntax and style checker using pep257."
;;                              :command ("pep257" source)
;;                              :error-patterns
;;                              ((warning line-start
;;                                        (file-name) ":" line " " (one-or-more not-newline) "\n        "
;;                                        (id (one-or-more (any alpha)) (one-or-more digit)) ": "
;;                                        (message (one-or-more not-newline))
;;                                        line-end))
;;                              :modes python-mode)
;;     (add-to-list 'flycheck-checkers 'python-pep257)
;;     (flycheck-add-next-checker 'python-flake8 'python-pylint)
;;     (flycheck-add-next-checker 'python-pep257 'python-flake8))

;; Ampl mode
(setq auto-mode-alist
      (cons '("\\.mod$" . ampl-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons '("\\.dat$" . ampl-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons '("\\.ampl$" . ampl-mode) auto-mode-alist))
(setq interpreter-mode-alist
      (cons '("ampl" . ampl-mode)
            interpreter-mode-alist))

(autoload 'ampl-mode "ampl-mode" "Ampl editing mode." t)

(provide 'matt-prog-other)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; matt-prog-other.el ends here
