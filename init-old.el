;;============================================================================;;
;;============================================================================;;
;;								 matt's init.el                               ;;
;;============================================================================;;
;;============================================================================;;

;;==============================================================================
;; SYSTEM SETUP
;;==============================================================================

;;set PATHs for Unix-based systems
(setenv "PATH" (concat (getenv "PATH") ":/usr/texbin"))
;; (setenv
;;  "PATH" (concat
;; 	 "/opt/:"
;; 	 "/opt/local/bin:"
;; 	 "/opt/local/sbin:"
;; 	 "/opt/X11/bin:"
;; 	 "/usr/local/bin:"
;; 	 "/usr/local/sbin:"
;; 	 "/usr/texbin:"
;; 	 "/usr/bin:"
;; 	 "/usr/sbin:"
;; 	 "/bin:"
;; 	 "/sbin:"))

(setq path-to-ctags "/opt/local/bin/ctags")

;; Set PYTHONPATH, because we don't load .bashrc
;;(setenv "PYTHONPATH" "/usr/local/lib/python2.7/site-packages:")

;; show stack trace on error
;;(setq debug-on-error t)

;; set file paths before anything else
;; set default search path
(setq default-directory "~/" )

;; set load path for extra packages
(add-to-list 'load-path "~/.emacs.d/lisp/")
;; (let ((default-directory "~/.emacs.d/lisp/"))
;;   (normal-top-level-add-subdirs-to-load-path))

;; M-x customize-* settings location
(setq custom-file "~/.emacs.d/custom-24.el")
(load custom-file)

;; features specific to emacs 24
(when (>= emacs-major-version 24)
  ;; theme path
  (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
  ;; initialize package manager
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.milkbox.net/packages/") t)
  ;; (add-to-list 'package-archives
  ;; 			   '("marmalade" . "http://marmalade-repo.org/packages/") t)
  )

;;==============================================================================
;;  KEYBINDINGS
;;==============================================================================

;; fix modifier keys on Mac GUI
;; aquamacs
;; (setq mac-command-modifier 'control ; L command -> C
;; 	  mac-option-modifier 'meta ; L option -> M
;; 	  mac-control-modifier 'super ; L control -> super
;; 	  mac-function-modifier 'hyper ; fn -> hyper
;; 	  mac-right-command-modifier 'control ; R command -> C
;; 	  mac-right-option-modifier 'meta) ; R option -> M

;; carbon emacs
(setq
 ;; Space-cadet keyboard mapping
 ns-command-modifier 'control ; L command -> C
 ns-option-modifier 'meta ; L option -> M
 ;;ns-control-modifier 'super ; L control -> super
 ;;ns-function-modifier 'hyper ; fn -> super
 ns-function-modifier 'meta

 ;; right hand side modifiers
 ns-right-command-modifier 'super ; R command -> super
 ns-right-option-modifier 'hyper ; R option -> hyper
 )

;; type brackets in pairs with Super and right hands's home-row
(global-set-key (kbd "s-j")
                (lambda () (interactive) (insert "()") (backward-char 1)))
(global-set-key (kbd "s-k")
                (lambda () (interactive) (insert "[]") (backward-char 1)))
(global-set-key (kbd "s-l")
                (lambda () (interactive) (insert "{}") (backward-char 1)))

(global-set-key (kbd "M-/") 'hippie-expand) ; replace default expand command
(global-set-key (kbd "<backtab>") 'hippie-expand) ; rebind said command to shift-tab

;;(ac-set-trigger-key "TAB")

;; Navigation
;; backwards then forwards
;; ----------
;; C-b C-f by char
;; M-b M-f by word
;; C-a C-e by line, laterally
;; C-p C-f by line, vertically
;; M-a M-e by sentence
;; M-[ M-] by paragraph
;; (M-{ and M-} is a pain in the ass)
(global-set-key (kbd "M-[") 'backward-paragraph)
(global-set-key (kbd "M-]") 'forward-paragraph)
;; M-v C-v by screen
;; M-< M-> start/end

;; Cut copy paste (there is no delete)
;; -----------------------------------
;; DEL backspace
;; C-d forward delete
;; M-d forward delete word
;; M-k forward delete sentence
;; C-w cut selection
;; M-w copy selection
;; C-y yank (paste)
;; M-y rotate kill ring
;; also alias to C-Y
;; (global-set-key (kbd "C-y") 'yank)
;; (global-set-key (kbd "C-Y") 'yank-pop)
;; ;; M-Y rotate kill ring orwards
;; (defun yank-pop-forwards (arg)
;;       (interactive "p")
;;       (yank-pop (- arg)))
;; (global-set-key (kbd "M-Y") 'yank-pop-forwards)

;; rebind normal search to use regex
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)

;; access normal isearch with meta key (though kind of useless)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; bind to compile
(global-set-key (kbd "C-x c") 'compile)

;; (global-set-key (kbd "C-x C-k")
;; 				(lambda () (interactive) (kill-buffer (current-buffer))))

;; cycle through split-panes
(global-set-key (kbd "C-c n")
                (lambda () (interactive) (select-window (next-window))))
(global-set-key (kbd "C-c p")
                (lambda () (interactive) (select-window (previous-window))))
;; cycle through frames (on the terminal, these fill the screen like tabs)
;; C-x 5 2 make new frame
;; C-x 5 b open buffer in new frame
;; C-x 5 f open file in new frame
;; C-x 5 o cycle to other frame
;; C-x 5 0 close frame

;; undo/redo
;; C-/ undo, C-? (C-shift-/) redo
;; (global-set-key (kbd "C-x u") 'undo)
(when (require 'redo nil 'noerror) (global-set-key (kbd "C-?") 'redo))
;; actually C-x u is undo but thats a hassle
;; (global-set-key (kbd "C-z") 'undo)
;; (when (require 'redo nil 'noerror) (global-set-key (kbd "C-Z") 'redo))

;; (global-set-key (kbd "<f4>") 'yank)       ; paste
;; (global-set-key (kbd "<C-f4>") 'yank-pop) ; paste previous
;; (global-set-key (kbd "<M-f4>") 'yank-pop-forwards)
;; (global-set-key (kbd "<f5>") 'undo)
;; (global-set-key (kbd "<C-f5>") 'redo)

(global-set-key (kbd "C-c f") 'other-frame)
;; easier split pane navigation
(if (window-system)
    (progn
      (global-set-key [M-left] 'windmove-left)          ; move to left windnow
      (global-set-key [M-right] 'windmove-right)        ; move to right window
      (global-set-key [M-up] 'windmove-up)              ; move to upper window
      (global-set-key [M-down] 'windmove-down))         ; move to lower window
  (progn
    (global-set-key (kbd "C-c <left>")  'windmove-left)
    (global-set-key (kbd "C-c <right>") 'windmove-right)
    (global-set-key (kbd "C-c <up>")    'windmove-up)
    (global-set-key (kbd "C-c <down>")  'windmove-down)))

;;(global-set-key (kbd "C-t") 'load-theme)



;; y/n prompts instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; smooth scrolling
(setq redisplay-dont-pause t
      scroll-margin 1
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1
      mouse-wheel-follow-mouse 't
      mouse-wheel-scroll-amount '(1 ((shift) . 1)))

;; because sometimes you want a mouse
;; (autoload 'xt-mouse "mouse in terminal")
;; (xterm-mouse-mode)

;; M-x aliases
(defalias 'ct 'load-theme)
(defalias 'sb 'sr-speedbar-toggle)
(defalias 'rs 'replace-string)
(defalias 'rex 'replace-regexp)

;;==============================================================================
;; STARTUP, UI, AND GENERAL SETTINGS
;;==============================================================================

;; show time on the mode line
(display-time)

;; kill UI cruft
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
;; want menu bar only in carbon emacs
(cond ((not window-system)
       (menu-bar-mode -1))
      ((and (window-system) (not (string-equal system-type "darwin")))
       (menu-bar-mode -1)))

;; no cursor blink
(blink-cursor-mode -1)

;; includes
(require 'cl) ;; use common lisp
(eval-when-compile (require 'cl))
(autoload 'saveplace "save last loc in file")
;;(autoload 'ffap "Finding Files and URLs at Point")
(autoload 'uniquify "unique buffer titles")
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
;; dired jump to current file dir (C-x C-j)
(autoload 'dired-x "dired-x")

(autoload 'smart-mode-line "sml")
(sml/setup)

;; isearch buffer switching
(iswitchb-mode 1)

;; ido
(autoload 'ido "ido")
(ido-mode t)
(eval-after-load "ido-mode"
  '(progn
     ;; fuzzy matching is a must have
     (setq ido-enable-flex-matching t)))

;; use ibuffer to list buffers by default
(defalias 'list-buffers 'ibuffer)

;; File formatting. yuck crlf
(prefer-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)

;; Global settings for ALL BUFFERS
(setq-default
 ;; Backups
 backup-by-copying t      ; don't clobber symlinks
 backup-directory-alist '(("." . "~/.saves"))    ; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t       ; use versioned backups

 ;; stop making foo~ files
 ;; make-backup-files nil

 ;; ;;store backup files in the system tmp directory
 ;; backup-directory-alist
 ;; 	  `((".*" . ,temporary-file-directory))
 ;; auto-save-file-name-transforms
 ;; 	  `((".*" ,temporary-file-directory t))

 ;; User interface
 ;; kill the splash screen and all that garbage
 inhibit-splash-screen t
 inhibit-startup-screen t
 inhibit-startup-buffer-menu t
 initial-scratch-message nil
 inhibit-startup-message t
 inhibit-startup-echo-area-message t

 ;; mode line customizations
 ;; display-battery-mode t
 line-number-mode t
 column-number-mode t

 ;; scrolling does not move cursor
 scroll-preserve-screen-position t
 ;; use wheel
 mouse-wheel-mode t

 ;; GLOBAL EDITING SETTINGS
 ;;
 ;; integrate with the system clipboard ffs
 x-select-enable-clipboard t
 ;; set initial major mode to be text
 initial-major-mode 'fundamental-mode
 ;; Increase number of undo
 undo-limit 100000
 ;; default fill-column is 80 chars
 fill-column 80
 ;; ispell is english
 ispell-dictionary "english"

 ;; Tabs and indentation and whitespace
 ;; ;; tabs to spaces by default
 ;; tabs are tabs by default
 indent-tabs-mode t
 ;; Default tab display is 4 spaces
 tab-width 4
 ;; default insert is also 4 and inc of 4
 ;; got to specify this or it will continue to expand to 8 spc
 ;;tab-stop-list (number-sequence 4 120 4)

 ;; No newlines at end of buffer unless I press return
 next-line-add-newlines nil
 ;; sentences end with one space only.
 sentence-end-double-space nil

 ;; FORCE FILES TO BE UTF-8 and LF damn it
 buffer-file-coding-system 'utf-8-unix
 default-file-name-coding-system 'utf-8-unix
 default-keyboard-coding-system 'utf-8-unix
 default-process-coding-system '(utf-8-unix . utf-8-unix)
 default-sendmail-coding-system 'utf-8-unix
 default-terminal-coding-system 'utf-8-unix

 ;; buffer handling
 save-place t
 save-place-forget-unreadable-files t
 uniquify-rationalize-file-buffer-names t
 uniquify-buffer-name-style 'forward

 ;; stfu and stop beeping. you ain't vim.
 ring-bell-function 'ignore
 ) ;; end startup setq


;; (setq initial-frame-alist
;;       '((menu-bar-lines . 0)
;;         (tool-bar-lines . 0)))

;; set scrollbar to right
;; (set-scroll-bar-mode 'right)

;; normal delete key behaviour please
;; highlight selection and overwrite
(delete-selection-mode t)
(transient-mark-mode t)

;; fonts (face) customization
(autoload 'faces "fonts")
;; default font size is 14pt on carbon emacs
(when (and (window-system)(string-equal system-type "darwin"))
  (set-face-attribute 'default nil
                      :font "Monaco" ;;andale mono
                      :height 120
                      :weight 'normal
                      :width 'normal))

(when (and (window-system)(string-equal system-type "gnu/linux"))
  (set-face-attribute 'default nil
                      :font "Monospace-10"
                      :height 100
                      :width 'normal))

;; icicles tab completion
;; (require 'icicles)
;; (icicle-mode t)

;; Default window width and height
;; (defun custom-set-frame-size ()
;;   (add-to-list 'default-frame-alist '(height . 65))
;;   (add-to-list 'default-frame-alist '(width . 99)))
;; (custom-set-frame-size)
;; (add-hook 'before-make-frame-hook 'custom-set-frame-size)

;; No popups and dialogues. They crash carbon emacs.
;; Not to mention that they're incredibly annoying.
(defadvice yes-or-no-p (around prevent-dialog activate)
  "Prevent yes-or-no-p from activating a dialog"
  (let ((use-dialog-box nil))
    ad-do-it))
(defadvice y-or-n-p (around prevent-dialog-yorn activate)
  "Prevent y-or-n-p from activating a dialog"
  (let ((use-dialog-box nil))
    ad-do-it))
;; Fallback. DIE, DIALOGUE BOXES, DIE!!
(setq use-dialog-box nil)

;; bloated IDE shit
;; autocomplete popup
;; (require 'auto-complete-config)
;; (add-to-list 'ac-dictionary-directories "~/.emacs.d/lisp/autocomplete//ac-dict")
;; (ac-config-default)

;; (require 'sr-speedbar) ;; in-window-speedbar

;;==============================================================================
;; BUFFER SAVING AND CLEANUP
;;==============================================================================
(desktop-save-mode 1) ;;save & autoload buffers
(setq desktop-dirname             "~/.emacs.d/desktop/"
      desktop-path                '("~/.emacs.d/desktop")
      desktop-base-file-name      "emacs-desktop"
      desktop-base-lock-name      "lock"
      desktop-save                nil
      desktop-files-not-to-save   "^$" ;reload tramp paths
      desktop-load-locked-desktop nil)
(setq desktop-buffers-not-to-save
      (concat "\\("
              "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
              "\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb"
              "\\)$"))
(add-to-list 'desktop-modes-not-to-save 'dired-mode)
(add-to-list 'desktop-modes-not-to-save 'Info-mode)
(add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
(add-to-list 'desktop-modes-not-to-save 'fundamental-mode)

(require 'midnight)

(eval-after-load "midnight"
  '(progn
     (setq
      ;; expiration date of all buffers is 1 day
      clean-buffer-list-delay-general 1
      ;;  expiration date of special buffers is 1 hour
      clean-buffer-list-delay-special (* 1 3600)
      )

     (defvar clean-buffer-list-timer nil
       "Stores clean-buffer-list timer if there is one.
You can disable clean-buffer-list by (cancel-timer clean-buffer-list-timer).")

     ;; run clean-buffer-list every 2 hours
     (setq clean-buffer-list-timer (run-at-time t (* 3 3600) 'clean-buffer-list))

     ;; kill everything, clean-buffer-list is very intelligent at not killing
     ;; unsaved buffers.
     (setq clean-buffer-list-kill-regexps
           '("^.*$"
             "\\`\\*Customize .*\\*\\'"
             "\\`\\*\\(Wo\\)?Man .*\\*\\'"))
     ;; ;; special buffers to be killed every 6 hours
     (add-to-list 'clean-buffer-list-kill-buffer-names
                  '("*bffer-selection*"
                    "*Finder*"
                    "*Finder Category*"
                    "*Finder-package*"
                    "*RE-Builder*"
                    "*vc-change-log*"))))

;; Invoking with M-x is easier
(defun close-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

(defun close-all-except-current-buffer ()
  (interactive)
  (mapc 'kill-buffer (cdr (buffer-list (current-buffer)))))

;;==============================================================================
;; Themes
;;==============================================================================

;; THEMES
;; set the theme depending on buffer.
;; requires the load-theme-buffer-local package
;; (when (>= emacs-major-version 24)
;;   ;; Cycle through this set of themes
;;   (cond ((window-system)
;; 		 (progn (setq my-themes '(spacegray tango-dark 'nil))
;; 				(setq my-cur-theme nil)))
;; 		((not window-system)
;; 		 (load-theme 'tango-dark t)))

;;   (defun cycle-my-theme ()
;; 	"Cycle through a list of themes, my-themes"
;; 	(interactive)
;; 	(when my-cur-theme
;; 	  (disable-theme my-cur-theme)
;; 	  (setq my-themes (append my-themes (list my-cur-theme))))
;; 	(setq my-cur-theme (pop my-themes))
;; 	(load-theme t my-cur-theme))

;;   ;; Switch to the first theme in the list above
;;   (when (window-system)
;; 	(cycle-my-theme))

;;   ;; Bind this to C-t
;;   (global-set-key (kbd "C-t") 'cycle-my-theme)
;;   (defun on-after-init ()
;; 	(unless (display-graphic-p (selected-frame))
;; 	  (set-face-background 'default "unspecified-bg" (selected-frame))))

;;   (add-hook 'window-setup-hook 'on-after-init)
;;   )

;; (cond
;;  ((and (>= emacs-major-version 24) (window-system))
;; 		(progn
;; 		  (add-hook 'prog-mode-hook (lambda () (enable-theme-buffer-local 'tango-dark (current-buffer))))
;; 		  (add-hook 'text-mode-hook (lambda () (enable-theme-buffer-local 'dichromacy (current-buffer))))
;; 		  ))
;; 	  ((and (>= emacs-major-version 24)(not window-system))
;; 		(load-theme 'tango-dark t)
;; 	  ))

;; only one theme at a time
(defadvice load-theme
  (before theme-dont-propagate activate)
  (mapc #'disable-theme custom-enabled-themes))

;; default theme on terminals
(when (not (window-system))
  (progn
    (load-theme 'wombat t)
    (set-background-color "black")))

;;==============================================================================
;; WRITING MODES
;;==============================================================================

;; General text mode
(autoload 'pandoc-mode
  "load pandoc generator minor mode when editing text" t nil)
(add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)

;; (add-hook 'text-mode-hook 'visual-line-mode)
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'text-mode-hook
          (progn
            (lambda()
              (global-linum-mode 0)
              (visual-line-mode 1)
              (setq
               ;; tabs to spaces in text mode
               indent-tabs-mode nil
               ;; Default tabs in text is 4 spaces
               tab-width 4
			   ;; default insert is also 4 and inc of 4
			   ;; got to specify this or it will continue to expand to 8 spc
			   tab-stop-list (number-sequence 4 120 4)
			   )
              ;; ask to turn on hard line wrapping
              (when (y-or-n-p "Hard wrap text? (auto-fill mode)")
                (turn-on-auto-fill)))))

;; LaTeX and AUCTeX
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)

;; Markdown
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(eval-after-load "markdown-mode"
  '(progn
     ;; markdown check paren balancing
     (add-hook 'markdown-mode-hook
               (lambda ()
                 (when buffer-file-name
                   (add-hook 'after-save-hook
                             'check-parens
                             nil t))))
     (add-hook 'markdown-mode-hook 'turn-on-pandoc)))

;; Org-mode
(setq org-startup-truncated nil)
(put 'upcase-region 'disabled nil)

;; multi-web-mode (html + js + css)
(autoload 'multi-web-mode "multi-web-mode" "" t)
(eval-after-load "multi-web-mode"
  '(progn
     (setq mweb-default-major-mode 'html-mode)
     (setq mweb-tags
           '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
             (js2-mode  "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
             (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
     (setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
     (multi-web-global-mode 1)))


;;==============================================================================
;; PROGRAMMING MODES
;;==============================================================================
;; line numbers
(autoload 'linum "linum-mode")
(eval-after-load "linum"
                                        ; one space separation, even in terminal
  (setq linum-format "%d "))

;; imenu function navigation
(setq imenu-auto-rescan t)

;; ctags
;; doesn't work on mac, xcode hijacks the path to macports ctags
;; (defun create-tags (dir-name)
;;   "Create tags file."
;;   (interactive "DDirectory: ")
;;   (shell-command
;;    (format "/opt/local/bin/ctags -e -R %s" (directory-file-name dir-name)))
;;   )

;; use etags
(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (eshell-command
   (format "find %s -type f -name \"*.[ch]\" | etags -" dir-name)))

;; auto indent
(when (>= emacs-major-version 24)
  (add-hook 'prog-mode-hook
            (lambda () (electric-indent-mode 1)))
  ;;yaml and python dont work with electeic-indent
  (add-hook 'yaml-mode-hook
            (lambda () (electric-indent-mode -1))))

;; highlight 80+ char overflows in programming modes
(autoload 'whitespace-mode "whitespace" "whitespace-mode" t nil)
;; (add-hook 'prog-mode-hook 'whitespace-mode)

;; highlights, line numbers, etc, common to all programming modes
(add-hook 'prog-mode-hook
          (progn
            'whitespace-mode
            'subword-mode
            'flyspell-prog-mode
            'flycheck-mode
            'flymake-mode
            (lambda()
              (global-linum-mode 1)
              (show-paren-mode 1)
              (hl-line-mode 1)
			  (add-hook 'before-save-hook 'delete-trailing-whitespace)
              ;; (define-key ac-mode-map (kbd "<backtab>") 'auto-complete)
              (setq
               ;; tabs are tabs when i'm programming, unless specified
               indent-tabs-mode t
               ;; also, ensure that tabs are 4 spc wide unless specified
               tab-width 4

			   subword-mode 1
               flymake-gui-warnings-enabled nil
               show-paren-delay 0
               ;; enable multiline comments
               comment-multi-line t
               ;; grep in colour
               grep-highlight-matches t
               ;; highlight 80 char overflows
               whitespace-line-column 80
               whitespace-style '(face lines-tail)))))

;; auto guess tabs or spaces
;; (autoload 'prog-mode "guess-style" "guess-style.el." t)
;;  (add-hook 'prog-mode-hook guess-style-guess-tabs-mode)
;;    (add-hook 'prog-mode-hook (lambda ()
;;                                   (when indent-tabs-mode
;;                                     (guess-style-guess-tab-width)))

(defun comment-auto-fill ()
  (setq-local comment-auto-fill-only-comments t)
  (auto-fill-mode 1))

;; compilation
;; Get compilation to understand javac/ant errors
(eval-after-load "compile"
  '(setq compilation-error-regexp-alist
         (append (list
                  ;; works for jikes
                  '("^\\s-*\\[[^]]*\\]\\s-*\\(.+\\):\\([0-9]+\\):\\([0-9]+\\):[0-9]+:[0-9]+:" 1 2 3)
                  ;; works for javac
                  '("^\\s-*\\[[^]]*\\]\\s-*\\(.+\\):\\([0-9]+\\):" 1 2))
                 compilation-error-regexp-alist)))

;;(global-set-key (kbd "TAB") 'self-insert-command)

;; Common Lisp
;; Setup load-path and autoloads
(autoload 'slime-autoloads "slime-autoloads")

;; Set lisp system and some contribs
(setq inferior-lisp-program (shell-command-to-string "which clisp"))
(setq slime-contribs '(slime-fancy slime-scratch slime-editing-commands))

;; MIT Scheme on Macs
;; (setenv "MITSCHEME_LIBRARY_PATH"
;;     "/Applications/mit-scheme.app/Contents/Resources")

(setq scheme-program-name "scheme")

(cond
 ((string-equal system-type "darwin")
  (progn
    (setq scheme-program-name
          "/Applications/mit-scheme.app/Contents/Resources/mit-scheme")
    (require 'xscheme))))

;; Scala
;; scala2-mode handles tabs to 2 spaces by default
(add-to-list 'auto-mode-alist '("\.scala" . scala-mode) '("\.sbt\'" . scala-mode))

;; Ocaml
(setq auto-mode-alist (cons '("\\.ml\\w?" . tuareg-mode) auto-mode-alist))
(autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
(autoload 'camldebug "camldebug" "Run the Caml debugger" t)
(add-hook 'tuareg-mode-hook
          (lambda()
            (setq indent-tabs-mode nil)))

;; Python
;; Fix the broken default py mode
(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))
(add-hook 'python-mode-hook
          (lambda()
            (setq-default indent-tabs-mode t
						  tab-width 4
						  ;;tab-stop-list (number-sequence 2 120 2)
						  py-indent-offset 4
						  ;;python-indent-offset 2
						  )))

;; javascript
(add-to-list 'auto-mode-alist '("\.js" . js2-mode))

;; C family common customizations
;; ------------------------------
;;
;; cc-mode hooks in order:
;; 1. c-initialization-hook, init cc mode once per session (i.e. emacs startup)
;; 2. c-mode-common-hook, run immediately before loading language hook
;; 3. then language hooks:
;;    c, c++, objc, java, idl, pike, awk

(add-hook 'c-initialization-hook
          (lambda ()
            (setq-default c-default-style '((c-mode . "linux")
											(c++-mode . "k&r")
                                            (java-mode . "java")
                                            (awk-mode . "awk")
                                            (other . "free-group-style"))
                          ;; set correct backspace behaviour
                          c-backspace-function 'backward-delete-char
						  ;; c-type lang specifics. want 4-space width tab tabs
						  c-basic-offset 4
						  c-indent-tabs-mode t
						  c-indent-level 4
						  c-tab-always-indent t
						  tab-width 4
						  ;; use tabs, not spaces.
						  indent-tabs-mode t)
            (add-to-list 'c-cleanup-list 'comment-close-slash)))

(add-hook 'c-mode-common-hook
		  (lambda ()
            ;; subword editing and movement to deal with CamelCase
			(c-toggle-electric-state 1)
			(subword-mode 1)
			(c-toggle-auto-newline 1)
			;; don't indent curly braces
            (c-set-offset 'statement-case-open 0)
            (c-set-offset 'substatement-open 0)
			(c-set-offset 'comment-intro 0)))

;; C
;; (add-hook 'c-mode-hook
;; 		  (lambda ()
;; 			(defun insert-block-comment ()
;;               (interactive)
;;               (insert "/**/")
;;               (backward-char 2))
;;             (local-set-key (kbd "C-c C-c") #'insert-block-comment)))

;; Java
;; jdee
;; (when (>= emacs-major-version 24)
;;   (add-to-list 'load-path "~/.emacs.d/lisp/jdee-2.4.1/lisp")
;;   (autoload 'jde-mode "jde" "JDE mode." t)
;;   (setq auto-mode-alist
;; 		(append '(("\\.java\\'" . jde-mode)) auto-mode-alist)))

;; (defun my-indent-setup ()
;;   (c-set-offset 'arglist-intro '+))
;; (add-hook 'java-mode-hook 'my-indent-setup)

(add-hook 'java-mode-hook
          (lambda ()
            "Treat Java 1.5 @-style annotations as comments."
            (setq c-comment-start-regexp "(@|/(/|[*][*]?))")
            (modify-syntax-entry ?@ "< b" java-mode-syntax-table)
		  (defun insert-block-comment ()
			(interactive)
			(insert "/***/")
			(backward-char 2))
		  (local-set-key (kbd "C-c C-c") #'insert-block-comment)
		  ;; insert javadoc comment
		  (defun java-insert-doc-comment () (interactive)
			(insert "/**\n * Description \n * @param \n * @return \n * @throws \n */"))
		  (defun java-insert-doc-header () (interactive)
			(insert "/**\n * Description \n * @author \n * @version \n */"))))

;;==============================================================================
;; MISC FUNCTIONS
;;==============================================================================
;; expand filled paragraph to a line
(defun unfill-paragraph ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

;; Yuck CRLF.
;; Function for converting from DOS to UNIX line ends
(defun unix-file ()
  "Change the current buffer to Latin 1 with Unix line-ends."
  (interactive)
  (set-buffer-file-coding-system 'utf-8-unix t))
(defun dos-file ()
  "Change the current buffer to Latin 1 with DOS line-ends."
  (interactive)
  (set-buffer-file-coding-system 'utf-8-dos t))
(defun mac-file ()
  "Change the current buffer to Latin 1 with Mac line-ends."
  (interactive)
  (set-buffer-file-coding-system 'utf-8-mac t))

;; M-x google!
(defun google ()
  "Google the selected region if any, display a query prompt otherwise."
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (url-hexify-string (if mark-active
                           (buffer-substring (region-beginning) (region-end))
                         (read-string "Google: "))))))

;; open the current file in another program. from emacs prelude.
(defun open-with (arg)
  "Open visited file in default external program.

With a prefix ARG always prompt for command to use."
  (interactive "P")
  (when buffer-file-name
    (shell-command (concat
                    (cond
                     ((and (not arg) (eq system-type 'darwin)) "open")
                     ((and (not arg) (member system-type '(gnu gnu/linux gnu/kfreebsd))) "xdg-open")
                     (t (read-shell-command "Open current file with: ")))
                    " "
                    (shell-quote-argument buffer-file-name)))))
(put 'erase-buffer 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
