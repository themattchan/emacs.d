;;; -*- lexical-binding: t; -*-
;;; matt-interface.el --- Startup, UI, and general settings.

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
;; Fix Emacs interface annoyances
(setq-default                     ; Global settings for ALL BUFFERS
 ;; kill the splash screen and all that garbage
 inhibit-splash-screen t
 inhibit-startup-message t
 inhibit-startup-screen t
 inhibit-startup-buffer-menu t
 initial-scratch-message ""

 ;;initial-buffer-choice                    ; Open to some file
 menu-prompting nil
 confirm-kill-emacs 'y-or-n-p ; Ask before exit

 ;;frame-title-format '(buffer-file-name "%f" "%b") ; Full file path in title
 display-warning-minimum-level 'error   ; Turn off annoying warning messages
 disabled-command-function nil          ; Don't second-guess advanced commands

 ;; mode line customizations
 battery-mode-line-format " [%L: %b%p%%] " ; %t for time
 display-battery-mode t

 display-time-default-load-average nil
 display-time-format "%a %d %b %l:%M %p"
 display-time-mode t

 line-number-mode t
 column-number-mode t

 ns-pop-up-frames nil ;; Mac open new files in the existing frame
 use-dialog-box nil

 ;; square cursor
 cursor-type 'box
 ;; scrolling does not move cursor
 scroll-preserve-screen-position t
 ;; use wheel
 mouse-wheel-mode t
 echo-keystrokes 0.1

 redisplay-dont-pause t

 ;; buffer handling
 save-place t
 save-place-forget-unreadable-files t
 uniquify-rationalize-file-buffer-names t
 uniquify-buffer-name-style 'forward
 buffers-menu-sort-function 'sort-buffers-menu-by-mode-then-alphabetically ; Buffers menu settings
 buffers-menu-grouping-function 'group-buffers-menu-by-mode-then-alphabetically
 buffers-menu-submenus-for-groups-p t
 ibuffer-default-sorting-mode 'filename/process
 ;; when using ido, the confirmation is rather annoying...
 confirm-nonexistent-file-or-buffer nil

 ;; font lock
 font-lock-use-fonts '(or (mono) (grayscale))    ; Maximal syntax highlighting
 font-lock-use-colors '(color)
 font-lock-maximum-decoration t
 font-lock-maximum-size nil
 font-lock-auto-fontify t

 ;; Smooth scrolling
 scroll-margin 1
 scroll-step 1
 scroll-conservatively 10000
 scroll-preserve-screen-position 1
 mouse-wheel-follow-mouse 't
 mouse-wheel-scroll-amount '(1 ((shift) . 1))

 ring-bell-function 'ignore
 ) ;; end startup setq

;; Redefine startup messasge
(defun startup-echo-area-message ()
  (format "Emacs started in %s." (emacs-init-time)))

;; y/n prompts instead of yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

;; No popups and dialogues. They crash carbon emacs.
;; Not to mention that they're incredibly annoying.
(advice-add 'y-or-n-p
            :around
            #'(lambda (y-or-n-p-fun prompt)
                (let ((use-dialog-box nil))
                  (funcall y-or-n-p-fun prompt))))

;; normal delete key behaviour please
(delete-selection-mode t)
;; highlight selection and overwrite
(transient-mark-mode t)

;;------------------------------------------------------------------------------
;; Kill UI cruft
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (not window-system) (menu-bar-mode -1))

;; No cursor blink
(blink-cursor-mode -1)

;;------------------------------------------------------------------------------
;; Load stuff
(eval-when-compile (require 'cl))
(use-package saveplace :defer t)
(use-package uniquify :defer 10)

;;------------------------------------------------------------------------------
;; ;; Show time on the mode line
;; (use-package display-time
;;   :defer 5
;;   :config
;;   (display-time))

;;------------------------------------------------------------------------------
;; Smart mode line
(use-package smart-mode-line
  :demand t
  :after faces
  :config
  (setq
   mode-line-format (delq 'mode-line-position mode-line-format)
   sml/theme 'respectful
   sml/shorten-modes t)
  (sml/setup))
;; '(:eval (if (use-region-p)
;;     (format "%d"
;;       (count-words-region (point) (mark)))
;;   (format "%d"
;;     (count-words-region (point-min) (point-max)))))
;; show battery

;; (add-hook 'after-init-hook #'fancy-battery-mode)
;; (fancy-battery-mode)
;; (setq-default fancy-battery-show-percentage t)
;; Start the server
;; (server-start)

;;------------------------------------------------------------------------------
;; Unclutter mode line

;; ;; Diminish settings without a home in use-package defns.
;; (diminish-minor-mode 'lisp-interaction-mode "λeval")
;; (diminish-minor-mode 'auto-complete-mode " α")
;; (diminish-minor-mode 'paredit-mode " π")
;; (diminish-minor-mode 'eldoc-mode "")
;; (diminish-minor-mode 'abbrev-mode "")
;; (diminish-minor-mode 'smartparens-mode "")
;; (diminish-minor-mode 'auto-highlight-symbol-mode "")
;; (diminish-minor-mode 'subword-mode "")
;; (diminish-minor-mode 'nxhtml-mode "nx")
;; (diminish-minor-mode 'button-lock-mode "")

;;------------------------------------------------------------------------------
;; Completion modes, etc
;; Use ibuffer to list buffers by default
(defalias 'list-buffers 'ibuffer)

;; isearch buffer switching
(use-package icomplete
  :defer t
  :config (icomplete-mode 1))

;; helm-mode
;; From Bryans's config
;; https://github.com/bryangarza/dot-emacs/blob/master/bryan/bryan-helm.el
(use-package helm
  :diminish
  :defer 2
  :init
  (progn
    (use-package helm-config)
    (global-unset-key (kbd "C-x c"))
    ) ;; end init

  :config
  (progn
    (when (executable-find "curl")
      (setq helm-net-prefer-curl t))
    (setq
     helm-input-idle-delay                 0.001
     helm-idle-delay                       0.001
     helm-candidate-number-limit           100
     helm-autoresize-max-height            40 ; it is %.
     helm-scroll-amount                    8
     helm-split-window-inside-p            t
     helm-move-to-line-cycle-in-source     t
     helm-ff-search-library-in-sexp        t
     helm-ff-file-name-history-use-recentf t
     helm-quick-update                     t
     helm-bookmark-show-location           t
     helm-ag-use-grep-ignore-list          t
     helm-ag-use-agignore                  t
     ;; fuzzy match
     helm-M-x-fuzzy-match                  t
     helm-ag-fuzzy-match                   t
     helm-buffers-fuzzy-matching           t
     helm-apropos-fuzzy-match              t
     helm-recentf-fuzzy-match              t
     helm-locate-fuzzy-match               t
     helm-file-cache-fuzzy-match           t
     helm-semantic-fuzzy-match             t
     helm-imenu-fuzzy-match                t
     helm-lisp-fuzzy-completion            t)

    (custom-set-variables
;;     '(helm-ag-base-command "rg --vimgrep --no-heading --smart-case ")
     '(helm-ag-base-command "ag --nocolor --nogroup --ignore-case")
     ;; flags are here
     ;; https://github.com/ggreer/the_silver_searcher/blob/682ab865e174ce289b7dda5514abfdf21037a2db/doc/ag.1.md
     '(helm-ag-command-option "--skip-vcs-ignores")
     '(helm-ag-insert-at-point 'symbol))

    (ido-mode -1) ; just in case
    (helm-mode)
    (helm-projectile-on)
    (helm-autoresize-mode t)
    (add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)
    ) ;progn

  :bind
  (:map helm-map
        ("<tab>" . helm-execute-persistent-action) ; rebind tab to run persistent action
        ("C-i"   . helm-execute-persistent-action) ; make TAB works in terminal
        ("C-z"   . helm-select-action)) ; list actions using C-z

  :bind
  (:map minibuffer-local-map
        ("C-c C-l" . helm-minibuffer-history))

  :bind
  (;; help
   ("C-h a"     . helm-apropos)
   ("C-h i"     . helm-info-emacs)
   ("C-h b"     . helm-descbinds)
   ("s-m"       . helm-man-woman)

   ;; super aliases
   ("s-f"       . helm-find-files)
   ("s-b"       . helm-mini)
   ("s-F"       . helm-occur)

   ;; rebind common keys, searching + browsing
   ("M-x"       . helm-M-x)
   ("M-y"       . helm-show-kill-ring)
   ("C-x b"     . helm-mini)
   ("C-x C-f"   . helm-find-files)
   ("C-x f"     . helm-recentf)
   ("C-x C-d"   . helm-browse-project)

   ("C-h SPC"   . helm-all-mark-rings)

   ;; silver searcher
   ("M-s s"     . helm-ag)
   ("M-s a"     . helm-ag-project-root)
   ("M-s b"     . helm-ag-buffers)
   ("M-s p"     . helm-ag-pop-stack)
   ("M-s C-c"   . helm-ag-clear-stack)
   ))

;;------------------------------------------------------------------------------
;; winner-mode
(use-package winner
  :defer t
  :config
  (winner-mode 1))

;;------------------------------------------------------------------------------
;; Recent files
(use-package recentf
  :defer 3
  :config
  (setq recentf-max-menu-items 30)
  (recentf-mode 1))

;;------------------------------------------------------------------------------
;; smart parens nav
;; (smartparens-global-mode t)

;;------------------------------------------------------------------------------
;; neotree

;; classic(default) ascii arrow icons nerd
(use-package neotree
  ;;  :bind
  ;;(bind-key "<f8>" 'neotree-toggle)
  :defer t
  :config
  (setq neo-theme 'nerd))

;;------------------------------------------------------------------------------
;; treemacs

(use-package treemacs
  :ensure t
  :defer t
  :init
  (progn
    (unbind-key "M-m" global-map))

  :config
  (progn
    (setq treemacs-follow-after-init          t
          treemacs-width                      35
          treemacs-indentation                2
          treemacs-collapse-dirs              (if (executable-find "python") 3 0)
          treemacs-silent-refresh             nil
          treemacs-change-root-without-asking nil
          treemacs-sorting                    'alphabetic-desc
          treemacs-show-hidden-files          t
          treemacs-never-persist              nil
          treemacs-is-never-other-window      nil
          treemacs-goto-tag-strategy          'refetch-index)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null (executable-find "python3"))))
      (`(t . t)
       (treemacs-git-mode 'extended))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ([f8]         . treemacs-toggle)
        ("M-0"        . treemacs-select-window)
        ("C-c 1"      . treemacs-delete-other-windows)
        ("M-m ft"     . treemacs-toggle)
        ("M-m fT"     . treemacs)
        ("M-m fB"     . treemacs-bookmark)
        ("M-m f C-t"  . treemacs-find-file)
        ("M-m f M-t"  . treemacs-find-tag)))

(use-package treemacs-projectile
  :defer t
  :ensure t
  :config
  (setq treemacs-header-function #'treemacs-projectile-create-header)
  :bind (:map global-map
              ("M-m fP" . treemacs-projectile)
              ("M-m fp" . treemacs-projectile-toggle)))

;;------------------------------------------------------------------------------
;; Themes

;; Only one theme at a time, auto disable prev loaded theme
(advice-add 'load-theme
            :around
            #'(lambda (load-theme-fun theme &rest args)
                (mapc #'disable-theme custom-enabled-themes)
                (apply load-theme-fun
                       (pcase args
                         (`(_ ,no-enable) (list theme t no-enable))
                         (_ (list theme t nil))))))

(load-file (locate-user-emacs-file "lisp-matt/badwolf-matt-theme.el"))

(if (window-system)
    (load-theme 'badwolf-matt) ; odersky
  (progn
    ;; default theme on terminals
    (load-theme 'wombat)
    (set-background-color "black")))

;;------------------------------------------------------------------------------
;; Face (fonts) customisation

(eval-and-compile
  (defun maximize-frame ()
    "Maximize the frame
(this is cribbed from the definition of toggle-frame-maximized)"
  (interactive)
  (let ((fullscreen (frame-parameter nil 'fullscreen)))
    (when (not (eq fullscreen 'maximized))
      (set-frame-parameter nil 'fullscreen 'maximized))))

(defsubst get-display-size ()
  (pcase (assq 'geometry (frame-monitor-attributes))
    (`(geometry ,x ,y ,w ,h) w)))

;; 13" macbook: 1280
;; retina mbp: 1680
;; home monitor: 1920
;; samsung ultrabook: 2560
(defun get-display-type ()
  (let ((w (get-display-size))
        (p (display-mm-height)))
    (cond
     ((and (>= w 2560) (>= p 350))  'hdpi)
     ((and (>= w 1680) (>= p 390)) 'macbookpro)
     ((>= w 1366) 'normal)
     ((>= w 1280) 'old-macbook)
     (t 'normal))))

(defun matt/font-size-for-display ()
  (case (get-display-type)
    ('hdpi 140) ;; 180 if hdpi mode is not turned on
    ('macbookpro 140)
    ('normal 120)
    ('old-macbook 100)))

(defun small-fonts ()
  (interactive)
  (set-face-attribute 'default nil :height (truncate (* 0.8 (matt/font-size-for-display)))))

(defun normal-fonts ()
  (interactive)
  (set-face-attribute 'default nil :height (matt/font-size-for-display)))

(defun large-fonts ()
  (interactive)
  (set-face-attribute 'default nil :height (truncate (* 1.2 (matt/font-size-for-display)))))

(defun extra-large-fonts ()
  (interactive)
  (set-face-attribute 'default nil :height (truncate (* 1.6 (matt/font-size-for-display)))))

(defun layout-4column ()
  (interactive)
;;  (small-fonts)
  (delete-other-windows)
  (dotimes (_ 3) (split-window-right))
  (balance-windows)
  (maximize-frame))

(defun layout-2by4 ()
  (interactive)
;;  (small-fonts)
  (delete-other-windows)
  (split-window-below)
  (dotimes (_ 3) (split-window-right))
  (other-window -1)
  (dotimes (_ 3) (split-window-right))
  (balance-windows)
  (maximize-frame))
) ;; eval-and-compile

;; default-frame-alist affects ALL frames
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(eval-and-compile
  (eval-when-compile (require 'cl))
  (defmacro font-alternatives (font &rest fonts)
    `(cond ,@(cl-mapcar #'(lambda (f) `((find-font (font-spec :name ,f)) ,f)) (cons font fonts)))))

(when (window-system)
  (set-face-attribute
   'default
   nil

   :font
   (font-alternatives "Hack" "Monaco" "Inconsolata" "PT Mono" "DejaVu Sans Mono" "Lucida Console" "Consolas")

   :height (matt/font-size-for-display)
   :weight 'normal
   :width 'normal))

;; (use-package faces
;;   :demand t
;;   ;;  :after helm
;;   :config
;;   (defun matt/default-fonts ()
;;     (interactive)
;;     (when (window-system)
;;       (set-face-attribute
;;        'default
;;        nil

;;        :font
;;        (font-alternatives "Hack" "Monaco" "Inconsolata" "PT Mono" "DejaVu Sans Mono" "Lucida Console" "Consolas")

;;        :height (matt/font-size-for-display)
;;        :weight 'normal
;;        :width 'normal))

;;     ;; Make the minibuffer/helm/modeline text small
;;     (let* ((prefixes '("mode-" "sml/")) ;"helm-"
;;            (smalls (seq-filter #'(lambda (face)
;;                                    (seq-some #'(lambda (pfx)
;;                                                  (string-prefix-p pfx (symbol-name face)))
;;                                              prefixes))
;;                                (face-list))))
;;       (mapc (lambda (face) (set-face-attribute face nil :height 120)) smalls)))
;;   (matt/default-fonts))

;;------------------------------------------------------------------------------
;; Projectile mode by default

(defconst my-globally-ignored-file-suffixes
  '( ".o"
     ".hi"
     ".out"
     ".elc"
     ".jar"
     ".class"
     ".pyc"
     ".gz"
     ".tar.gz"
     ".tgz"
     ".zip"
     ".bak"
     ".log"
     ))
(defconst my-globally-ignored-files
  '( ".DS_Store"
     "*~"
     "\#*\#"
     "#*#"
     "yarn.lock"
     "package-lock.json"
     "TAGS"
     ))

(defconst my-globally-ignored-directories
  '("*.liquid" ".stack-work" "dist" "out"
    "repl" "target" "venv" "tmp"
    "output" "node_modules" "bower_components"
    ))

(defconst my-root-files
  '("bower.json" "package.json" "TAGS" "*.cabal"))

(use-package grep
  :defer 5in
  :config
  (setq grep-highlight-matches t)   ; grep in colour
  (setq grep-find-ignored-files
        (append (mapcar #'(lambda (x) (concat "*" x)) my-globally-ignored-file-suffixes)
                (append my-globally-ignored-files grep-find-ignored-files)))
  (setq grep-find-ignored-directories
        (append my-globally-ignored-directories
                grep-find-ignored-directories))
  )

(use-package projectile
  :diminish
  :defer 5
  :ensure t
  :config

  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

  (setq projectile-globally-ignored-file-suffixes
        (append
         my-globally-ignored-file-suffixes
         projectile-globally-ignored-file-suffixes))

  (setq projectile-globally-ignored-files
        (append
         my-globally-ignored-files
         projectile-globally-ignored-files))

  ;; https://github.com/bbatsov/projectile/pull/1153/files
  ;; "if the directory is prefixed with '*' then ignore all directories matching that name"
  (setq projectile-globally-ignored-directories
        (append
         my-globally-ignored-directories
         projectile-globally-ignored-directories))

  (setq projectile-project-root-files
        (append my-root-files
                projectile-project-root-files))
  (setq projectile-project-root-files-bottom-up
        (append projectile-project-root-files
                projectile-project-root-files-bottom-up))

  (defun projectile-find-my-root (start-dir)
    (when (not (or (string= (substitute-in-file-name "$HOME/") start-dir)
                   (string= "~/" start-dir)))
      (let ((search-here (file-expand-wildcards (concat start-dir "*.cabal"))))
        (if search-here
            (file-name-directory (car search-here))
          (projectile-find-my-root (file-name-directory (directory-file-name start-dir)))))))

;;  (add-to-list 'projectile-project-root-files-functions #'projectile-find-my-root)
  (setq projectile-project-root-files-functions
        '(projectile-find-my-root
          projectile-root-bottom-up
          projectile-root-local
          projectile-root-top-down
          projectile-root-top-down-recurring))
  (projectile-mode)
  )

;;------------------------------------------------------------------------------
;; anzu mode (show current & total matches in isearch)

(use-package anzu
  :diminish
  :ensure t
  :demand t
  :bind
  (("M-%" . anzu-query-replace)
   ("C-M-%" . anzu-query-replace-regexp))
  :config
  (global-anzu-mode))


(provide 'matt-interface)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; matt-interface.el ends here
