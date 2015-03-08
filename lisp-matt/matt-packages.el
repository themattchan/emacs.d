(defvar matt/packages
  '(;; essential utilities
    auto-complete
    auto-complete-auctex
    ac-c-headers
    ac-etags
    ac-geiser
    ac-ispell
    ac-math
    ac-slime

    ag                                  ; silver searcher for emacs
    autopair
    bind-key
    ctags
    ctags-update
    ecb                                 ; emacs code browser
    feature-mode
    flycheck
    flyspell
    ;; flyspell-prog-mode
    gist
    htmlize
    magit
    marmalade
    paredit
    spacegray-theme                     ; favourite dark theme
    sublime-themes                      ; a collection of clean themes (named
                                        ; after famous programmers), good for
                                        ; programming
    soft-stone-theme                    ; beige theme, good for writing
    use-package

    ;; major modes
    auctex
    bison-mode
    lua-mode
    org
    clojure-mode
;   cider
    coffee-mode
    sbt-mode
    scala-mode                          ; idk which scala mode is actually being used...
    scala-mode2
    go-mode
    graphviz-dot-mode
    haml-mode
    haskell-mode
    shm                                 ; structured-haskell-mode
    sml-mode
    web-mode
    writegood-mode
    markdown-mode
    yaml-mode)
  "Default packages")

(defun matt/packages-installed-p ()
  (loop for pkg in matt/packages
        when (not (package-installed-p pkg)) do (return nil)
        finally (return t)))

(provide 'matt-packages)
