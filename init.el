;;==============================================================================
;;==============================================================================
;;                               matt's init.el
;;==============================================================================
;;==============================================================================

;;==============================================================================
;; Personalisations and globals
;;==============================================================================
(setq user-full-name "Matthew Chan"
      user-mail-address "matt@themattchan.com")
(defconst *is-a-mac* (eq system-type 'darwin))
(defconst *is-linux* (member system-type '(gnu gnu/linux gnu/kfreebsd)))
(defconst *is-windows* (member system-type '(ms-dos windows-nt cygwin)))

;;==============================================================================
;; Setup paths
;;==============================================================================
(eval-when-compile (require 'cl))       ; use common lisp (macros only)
(require 'cl-lib)

(defvar *my-path-list*
  '("/opt/"
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
    "/Users/matt/Library/Haskell/bin"        ; for hdevtools
    ))

;; set PATHs for Unix-based systems
(setenv "PATH" (mapconcat #'identity *my-path-list* ":"))
(setq exec-path (append exec-path *my-path-list*))

(when *is-a-mac* (setq path-to-ctags "/opt/local/bin/ctags"))

;; Set PYTHONPATH, because we don't load .bashrc
;;(setenv "PYTHONPATH" "/usr/local/lib/python2.7/site-packages:")

;; show stack trace on error
;; (setq debug-on-error t)

;; set file paths before anything else
;; set default search path
(setq default-directory "~/Dropbox/" )

;; set load path for extra packages
(add-to-list 'load-path "~/.emacs.d/lisp-matt/")
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; (let ((default-directory "~/.emacs.d/lisp/"))
;;   (normal-top-level-add-subdirs-to-load-path))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

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

;; M-x customize-* settings location
(setq custom-file "~/.emacs.d/custom-24.el")
(load custom-file)

;; Required packages. Never leave home without them.
(require 'matt-packages)
(unless (matt/packages-installed-p)
  (message "%s" "Installing required packages...")
  (package-refresh-contents)
  (dolist (pkg matt/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

(require 'use-package)                  ; also provides bind-key
(require 'saveplace)
(require 'uniquify)
(require 'ansi-color)
(require 'recentf)

;;==============================================================================
;; Now load my configs
;;==============================================================================
(setq load-prefer-newer t)          ; Load latest bytecode

(let ((matt-configs
       '(;; Emacs Lisp functions
         matt-elisp-func             ; Require first! Functions get used later

         ;; General interface settings
         matt-keybindings            ; Fix Emacs annoyances, add power
         matt-interface              ; Fix Emacs annoyances, add power
         matt-edit-all               ; Tabs, fill, undo, ispell, UTF-8, backups
         matt-themes                 ; Mostly deprecated

         ;; Hooks for editing
         ;; Text documents
         matt-documents              ; Text: Markdown, LaTeX, Org-mode
         matt-html                   ; Web markup: HTML, CSS

         ;; Programming
         matt-prog-all               ; Settings for all programming modes
         matt-prog-functional        ; Settings for all functional langs
         matt-prog-lisp              ; Lisp family: CL, Scheme, Racket, Clojure
         matt-prog-ml                ; ML family: Haskell, OCaml
         matt-prog-cc                ; C family: C, C++, Java, (and Asm)
         matt-prog-other             ; Other langs: Scala, Python, web stuff

         ;; Misc
         matt-utils                  ; System utilities: terminal, dired...
         )))

  (dolist (config matt-configs)
    (require config)))

;;(message "%s" (emacs-init-time))
