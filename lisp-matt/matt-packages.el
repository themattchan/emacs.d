;;; matt-packages.el --- A list of required packages.

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

;; TODO: find a way to auto-generate this from the currently installed package
;; list.

;;; Code:

;; EXPORTED 6/6/15
;; Don't know what half of this shit is...
;; C-h v package-activated-list
(defvar matt/packages
  '(ac-c-headers
    ac-cider
    ac-etags
    ac-geiser
    ac-haskell-process
    ac-ispell
    ac-math
    ac-slime
    adjust-parens
    ag
    anything
    anything-exuberant-ctags
    anything-git-files
    anything-git-grep
    async
    auctex
    auctex-latexmk
    auctex-lua
    auto-complete
    auto-complete-auctex
    autopair
    bind-key
    bison-mode
    button-lock
    caml
    cider
    clojure-mode
    clojure-test-mode
    coffee-mode
    color-theme
    company
    company-auctex
    company-c-headers
    company-cabal
    company-coq
    company-ghc
    company-ghci
    company-math
    company-web
    concurrent
    ctable
    ctags
    ctags-update
    dash
    deferred
    diminish
    dired+
    dired-details
    dired-details+
    direx
    dirtree
    display-theme
    django-mode
    dot-mode
    ecb
    ein
    elm-mode
    elpy
    elscreen
    emamux
    ensime
    epc
    epl
    ess
    ess-R-data-view
    ess-R-object-popup
    ess-smart-equals
    ess-smart-underscore
    evil
    exec-path-from-shell
    expand-region
    f
    faceup
    feature-mode
    fill-column-indicator
    find-file-in-project
    flx
    flx-ido
    flycheck
    flycheck-clojure
    flycheck-color-mode-line
    flycheck-elm
    flycheck-ghcmod
    flycheck-haskell
    flycheck-hdevtools
    flycheck-liquidhs
    flycheck-ocaml
    flycheck-pos-tip
    flylisp
    flymake-easy
    flymake-haskell-multi
    flymake-hlint
    flymake-python-pyflakes
    flymake-racket
    flyparens
    fold-dwim
    fold-dwim-org
    framesize
    fringe-helper
    furl
    fuzzy
    geiser
    gh
    ghc
    gist
    git
    git-annex
    git-auto-commit-mode
    git-blame
    git-commit
    git-commit-mode
    git-rebase-mode
    gitconfig
    gitignore-mode
    gitlab
    go-mode
    goto-chg
    graphviz-dot-mode
    haml-mode
    haskell-mode
    helm
    helm-ag
    helm-ag-r
    helm-company
    helm-core
    helm-flycheck
    helm-flyspell
    helm-ghc
    helm-git
    helm-gitignore
    helm-gitlab
    helm-ls-git
    helm-ls-hg
    helm-ls-svn
    helm-make
    highlight-indentation
    hippie-expand-slime
    htmlize
    hy-mode
    hydra
    icicles
    ido-at-point
    ido-complete-space-or-hyphen
    ido-completing-read+
    ido-hacks
    ido-load-library
    ido-ubiquitous
    idomenu
    iedit
    igrep
    image+
    impatient-mode
    jedi
    jedi-core
    jedi-direx
    js2-mode
    js3-mode
    jtags
    julia-mode
    key-chord
    keyfreq
    latex-pretty-symbols
    latex-preview-pane
    let-alist
    lex
    liquid-tip
    list-utils
    load-theme-buffer-local
    logito
    lua-mode
    magit
    magit-popup
    markdown-mode
    marmalade
    math-symbol-lists
    memory-usage
    menu-bar+
    merlin
    moe-theme
    monokai-theme
    multi-term
    multi-web-mode
    neotree
    nose
    org
    org-journal
    org-pandoc
    osx-plist
    pandoc-mode
    paredit
    pcache
    persistent-soft
    pkg-info
    popup
    pos-tip
    projectile
    psci
    purescript-mode
    python-environment
    pyvenv
    quack
    queue
    racket-mode
    rainbow-delimiters
    request
    rich-minority
    s
    sbt-mode
    scala-mode
    scala-mode2
    scala-outline-popup
    scf-mode
    scheme-complete
    seq
    seti-theme
    shell-command
    shm
    sicp
    simple-httpd
    slime
    smart-mode-line
    smartparens
    sml-mode
    soft-stone-theme
    solarized-theme
    spacegray-theme
    spaces
    spinner
    spotify
    sublime-themes
    swiper
    tabbar
    top-mode
    tree-mode
    tuareg
    undo-tree
    use-package
    visual-fill-column
    w3
    wc-goal-mode
    wc-mode
    web-completion-data
    web-mode
    websocket
    windata
    with-editor
    wolfram-mode
    writegood-mode
    writeroom-mode
    yaml-mode
    yasnippet
    zenburn-theme)
  "Default packages")

(defun matt/packages-installed-p ()
  (loop for pkg in matt/packages
        when (not (package-installed-p pkg))
        do (return nil)
        finally (return t)))

(provide 'matt-packages)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; matt-packages.el ends here
