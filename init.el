;;; init.el --- Top-level initialisation.

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

(defun matt/recompile-settings ()
  (interactive)
  (byte-recompile-file "~/.emacs.d/init.el" 0)
  (byte-recompile-file "~/.emacs.d/custom-24.el" 0)
  (byte-recompile-directory "~/.emacs.d/lisp-matt" 0)
  (load-file "~/.emacs.d/init.elc"))

;;==============================================================================
;; Personalisations and globals
;;==============================================================================

(setq user-full-name      "Matthew Chan"
      user-mail-address   "matt@themattchan.com")


(defconst *is-mac*     (eq system-type 'darwin))
(defconst *is-linux*   (member system-type '(gnu gnu/linux gnu/kfreebsd)))
(defconst *is-windows* (member system-type '(ms-dos windows-nt cygwin)))

;;==============================================================================
;; Setup paths
;;==============================================================================
(eval-when-compile (require 'cl))       ; use common lisp (macros only)
(require 'cl-lib)

(setq *my-path-list*
  `("/opt/"
    "/opt/local/bin"
    "/opt/local/sbin"
    "/opt/X11/bin"
    "/usr/local/bin"
    "/usr/local/sbin"
    "/usr/texbin"
    "/usr/bin"
    "/usr/sbin"
    "/bin"
    "/sbin"
    ,(substitute-in-file-name "$HOME/Library/Haskell/bin") ; for hdevtools, ghc-mod
    ,(substitute-in-file-name "$HOME/.local/bin")          ; stack install dir
    "/Library/TeX/texbin"
    ))

;; Set PATHs for Unix-based systems
(setenv "PATH" (mapconcat #'identity *my-path-list* ":"))
(setq exec-path (append exec-path *my-path-list*))

(when *is-mac* (setq path-to-ctags "/opt/local/bin/ctags"))

;; Set PYTHONPATH, because we don't load .bashrc
;;(setenv "PYTHONPATH" "/usr/local/lib/python2.7/site-packages:")

;; Show stack trace on error
;; (setq debug-on-error t)

;; Set file paths before anything else

;; load path for extra packages
(add-to-list 'load-path (concat user-emacs-directory "lisp-matt/"))
;;(add-to-list 'load-path (concat user-emacs-directory "lisp/"))
(let ((default-directory  (concat user-emacs-directory "lisp/")))
  (normal-top-level-add-to-load-path '("flycheck-liquidhs.el" "liquid-types.el")))
;; customize.el settings location
(setq custom-file "~/.emacs.d/custom-24.el")
(load custom-file)

;;==============================================================================
;; And packages
;;==============================================================================

(require 'package)

;; (add-to-list 'package-archives
;;              '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(setq package-archive-enable-alist '(("melpa" deft magit)))
(package-initialize)

;; Required packages. Never leave home without them.
(require 'matt-packages)
(unless (matt/packages-installed-p)
  (message "%s" "Installing required packages...")
  (package-refresh-contents)
  (dolist (pkg matt/packages)
    (when (not (package-installed-p pkg))
      (ignore-errors
        (package-install pkg)))))

(require 'use-package)                  ; also provides bind-key
(require 'saveplace)                    ; what is this stuff...
(require 'uniquify)
(require 'ansi-color)
(require 'recentf)

;;==============================================================================
;; Now load my configs
;;==============================================================================
(setq load-prefer-newer t)           ; Load latest bytecode

(load-file "~/.emacs.d/private.el")

(let ((matt-configs
       '(;; Emacs Lisp functions
         matt-elisp-func             ; Require first! Functions get used later

         ;; General interface settings
         matt-keybindings            ; Fix Emacs annoyances, add power
         matt-interface              ; Fix Emacs annoyances, add power
         matt-edit-global               ; Tabs, fill, undo, ispell, UTF-8, backups
         matt-themes                 ; Mostly deprecated

         ;; Hooks for editing
         ;; Text documents
         matt-documents              ; Text: Markdown, LaTeX, Org-mode
         matt-web                    ; Web markup: HTML, CSS

         ;; Programming
         matt-prog-global             ; Settings for all programming modes
         matt-prog-functional         ; Settings for all functional langs
         matt-prog-lisp               ; Lisp family: CL, Scheme, Racket, Clojure
         matt-prog-haskell            ; Haskell
         matt-prog-ml                 ; ML family: OCaml
         matt-prog-cc                 ; C family: C, C++, Java, (and ASM)
         matt-prog-other              ; Other langs: Scala, Python, web stuff

         ;; Misc
         matt-utils                     ; System utilities: terminal, dired...
         )))

  (dolist (config matt-configs)
    (require config)
    (message "+ Loaded %s" config)))

;; (message "Emacs started in %s." (emacs-init-time))

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; init.el ends here
