
(defvar matt/packages (quote (ac-c-headers ac-cider ac-etags ac-geiser ac-haskell-process ac-ispell ac-math ac-slime adjust-parens ag anything-exuberant-ctags anything-git-files anything-git-grep anything auctex-latexmk auctex-lua auto-complete-auctex autopair bison-mode button-lock cfengine-code-style coffee-mode color-theme company-auctex auctex company-c-headers company-cabal company-coq company-ghc company-ghci company-math company-web ctags-update dante dash-functional dired+ dired-details+ dired-details dirtree display-theme django-mode doom-themes all-the-icons dot-mode dr-racket-like-unicode ecb ein electric-case elm-mode elpy elscreen emamux ensime exec-path-from-shell expand-region feature-mode fill-column-indicator find-file-in-project flx-ido flx flycheck-clojure cider clojure-mode flycheck-color-mode-line flycheck-elm flycheck-ghcmod flycheck-haskell flycheck-hdevtools flycheck-liquidhs flycheck-ocaml flycheck-pos-tip flycheck-purescript flylisp flymake-haskell-multi flymake-hlint flymake-python-pyflakes flymake-easy flyparens fold-dwim-org fold-dwim font-lock+ framesize fringe-helper fsharp-mode company-quickhelp furl fuzzy geiser gist gh git git-annex git-auto-commit-mode gitconfig go-mode goto-chg graphviz-dot-mode haml-mode helm-ag helm-ag-r helm-company helm-flycheck helm-flyspell helm-ghc ghc helm-git helm-gitignore gitignore-mode helm-gitlab gitlab helm-hoogle helm-idris helm-ls-git helm-ls-hg helm-ls-svn helm-make helm helm-core highlight-indentation hippie-expand-slime hy-mode icicles ido-at-point ido-complete-space-or-hyphen ido-hacks ido-load-library ido-ubiquitous ido-completing-read+ idomenu idris-mode iedit igrep image+ impatient-mode htmlize intero company jedi-direx direx jedi auto-complete jedi-core epc ctable concurrent js3-mode json-mode json-reformat json-snatcher jtags julia-mode key-chord keyfreq latex-pretty-symbols latex-preview-pane lex load-theme-buffer-local logito lua-mode magit git-commit magit-popup markdown-mode marshal ht math-symbol-lists memoize memory-usage menu-bar+ merlin moe-theme monokai-theme multi-term multi-web-mode neotree nginx-mode nix-buffer nix-mode nix-sandbox nlinum nodejs-repl nose olivetti org-journal osx-plist pandoc-mode hydra paredit persistent-soft list-utils pcache popup pos-tip projectile prop-menu psci f purescript-mode python-environment python-mode pyvenv quack queue racket-mode faceup rainbow-delimiters request-deferred request deferred s sbt-mode scala-mode scf-mode scheme-complete seti-theme shell-command shm sicp skewer-mode js2-mode simple-httpd slime macrostep smart-mode-line rich-minority smartparens sml-mode soft-stone-theme solarized-theme spacegray-theme spacemacs-theme spaces spinner spotify stack-mode flycheck seq pkg-info epl dash haskell-mode stripe-buffer sublime-themes swiper ivy tabbar top-mode tree-mode tuareg caml undo-tree use-package diminish bind-key utop w3 wc-goal-mode wc-mode web-completion-data web-mode websocket windata with-editor async wolfram-mode writegood-mode writeroom-mode visual-fill-column yaml-mode yasnippet zenburn-theme znc)) "Default packages")

(defun matt/packages-installed-p nil (loop for pkg in matt/packages when (not (package-installed-p pkg)) do (return nil) finally (return t)))

(provide (quote matt-packages))
