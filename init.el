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

;; from John Wiegley's setup
(setq message-log-max 16384
      gc-cons-threshold 500000000       ; 500mb
      gc-cons-percentage 0.6)

(add-hook 'after-init-hook
          `(lambda ()
             (setq gc-cons-threshold 50000000 ; 50mb
                   gc-cons-percentage 0.4)
             (garbage-collect)) t)

(eval-and-compile
  (defun matt/recompile-settings ()
    (interactive)
    (byte-recompile-file "~/.emacs.d/init.el" t 0)
    (byte-recompile-file "~/.emacs.d/custom.el" t 0)
    (byte-recompile-directory "~/.emacs.d/lisp-matt/" 0 t)
    (load-file user-init-file)
    ))

;;==============================================================================
;; Personalisations and globals
;;==============================================================================

(setq user-full-name      "Matthew Chan"
      user-mail-address   "matt@themattchan.com")

(defconst *is-mac*     (eq system-type 'darwin))
(defconst *is-linux*   (member system-type '(gnu gnu/linux gnu/kfreebsd)))
(defconst *is-windows* (member system-type '(ms-dos windows-nt cygwin)))

;;==============================================================================
;; Packages
;;==============================================================================
(setq load-prefer-newer t)           ; Load latest bytecode

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '(("melpa" . "http://melpa.org/packages/")
               ("gnu" . "httpL//elpa.gnu.org/packages/")))
(setq package-user-dir (concat user-emacs-directory "elpa/"))
(setq package-archive-enable-alist '(("melpa" deft magit)))
(package-initialize nil)

(eval-and-compile
  (defun matt/install-my-packages ()
    (message "Installing required packages...")
    (package-refresh-contents)
    (dolist (pkg package-selected-packages)
      (when (not (package-installed-p pkg))
        (message "  + Installing package: %s" pkg)
        (ignore-errors
          (package-install pkg))))))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile (require 'cl))       ; use common lisp (macros only)
(eval-when-compile
  (add-to-list 'load-path (car (directory-files "~/.emacs.d/elpa" nil "use-package-*" nil)))
  ;; also provides bind-key
  (require 'use-package))

(setq use-package-always-ensure nil)
(setq use-package-always-defer t)

(use-package diminish :ensure t :demand t :defer nil)
(use-package bind-key :ensure t :demand t :defer nil)
(use-package cl-lib :ensure t)
(use-package f :ensure t)      ; filesystem utils
(use-package s :ensure t)      ; string utils

;;==============================================================================
;; Setup paths
;;==============================================================================

(setenv "SHELL" "/bin/bash")
(setenv "TERM" "xterm-256color")
(setenv "LC_CTYPE" "en_US.UTF-8")
(setenv "LC_ALL" "en_US.UTF-8")

(setq *my-path-list*
      `("~/.nix-profile/bin"
        "~/.nix-profile/sbin"
        "/opt/"
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
(setenv "NIX_PATH" (substitute-in-file-name "$HOME/.nix-defexpr/channels/nixpkgs/"))

;; Set PATHs for Unix-Based systems
(setenv "PATH" (mapconcat #'identity *my-path-list* ":"))
(setq exec-path (append exec-path *my-path-list*))

(when *is-mac* (setq path-to-ctags "/opt/local/bin/ctags"))

;; Set PYTHONPATH, because we don't load .bashrc
;;(setenv "PYTHONPATH" "/usr/local/lib/python2.7/site-packages:")

;; Show stack trace on error
;;(setq debug-on-error t)

;; Set file paths before anything else

;; load path for extra packages
(add-to-list 'load-path (concat user-emacs-directory "lisp-matt/"))
(add-to-list 'load-path (concat user-emacs-directory "lisp/"))
;; (let ((default-directory  (concat user-emacs-directory "lisp/")))
;;   (message "load lisp files")
;;   (dolist (file (directory-files default-directory nil "\\.el$"))
;;     (when (file-regular-p file)
;;       (message "loaded %s" file)
;;       (load-file (concat  (file-name-as-directory default-directory) file))))
;;   (normal-top-level-add-to-load-path '("flycheck-liquidhs.el" "liquid-types.el")))

;; customize.el
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;;==============================================================================
;; Now load my configs
;;==============================================================================
(let ((private "~/.emacs.d/private.el"))
  (when (file-exists-p private)
    (load-file private)))

(let ((matt-configs
       '(;; Emacs Lisp functions
         matt-elisp-func             ; Require first! Functions get used later

         ;; General interface settings
         matt-keybindings
         matt-interface
         matt-edit-global
         matt-themes

         ;; Hooks for editing
         ;; Text documents
         matt-documents              ; Text: Markdown, LaTeX, Org-mode
         matt-web                    ; Web markup: HTML, CSS

         ;; Programming
         matt-prog-global             ; Settings for all programming modes
         matt-prog-functional         ; Settings for all functional langs
         matt-prog-haskell            ; Haskell
         matt-prog-ml                 ; ML family: OCaml
         matt-prog-lisp               ; Lisp family: CL, Scheme, Racket, Clojure
         matt-prog-cc                 ; C family: C, C++, Java, (and ASM)
         matt-prog-other              ; Other langs: Scala, Python, web stuff

         ;; Misc
         matt-utils                   ; System utilities: terminal, dired...
         )))

  (dolist (config matt-configs)
    (require config)
    (message "+ Loaded %s" config)))

(message "Emacs started in %s." (emacs-init-time))

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; init.el ends here
