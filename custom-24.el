(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-command "latex")
 '(TeX-PDF-mode t)
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#0d0d0d" "#cc6666" "#b5bd68" "#f0c674" "#81a2be" "#c9b4cf" "#8abeb7" "#ffffff"])
 '(ansi-term-color-vector
   [default "#151718" "#CE4045" "#9FCA56" "#DCCD69" "#55B5DB" "#A074C4" "#55B5DB" "#D4D7D6"] t)
 '(asm-comment-char 33)
 '(blink-cursor-mode nil)
 '(browse-url-browser-function (quote eww-browse-url))
 '(browse-url-text-browser "eww")
 '(c-block-comment-prefix (quote set-from-style))
 '(c-comment-prefix-regexp (quote set-from-style))
 '(c-doc-comment-style
   (quote
    ((c-mode . gtkdoc)
     (java-mode . javadoc)
     (pike-mode . autodoc))))
 '(comment-multi-line t)
 '(comment-style (quote multi-line))
 '(company-coq-prettify-symbols (quote f))
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(cursor-type (quote box))
 '(custom-safe-themes
   (quote
    ("4a7abcca7cfa2ccdf4d7804f1162dd0353ce766b1277e8ee2ac7ee27bfbb408f" "78559045fb299f3542c232166ad635c59cf0c6578d80a58b885deafe98a36c66" "3e335d794ed3030fefd0dbd7ff2d3555e29481fe4bbb0106ea11c660d6001767" "cc0dbb53a10215b696d391a90de635ba1699072745bf653b53774706999208e3" "10e3d04d524c42b71496e6c2e770c8e18b153fcfcc838947094dad8e5aa02cef" "d2c61aa11872e2977a07969f92630a49e30975220a079cd39bec361b773b4eb3" "31e64af34ba56d5a3e85e4bebefe2fb8d9d431d4244c6e6d95369a643786a40e" "4b207752aa69c0b182c6c3b8e810bbf3afa429ff06f274c8ca52f8df7623eb60" "a17c68ed6c3feb24eaeb7bb4d69221e0cf3e6a658f1da62c8f8edb90769b7b12" "7c3a6181ad96ae8cbe2414d7a45c999e46230310a64ea9fa86c6e8c46f7257c3" "c4bd8fa17f1f1fc088a1153ca676b1e6abc55005e72809ad3aeffb74bd121d23" "527df6ab42b54d2e5f4eec8b091bd79b2fa9a1da38f5addd297d1c91aa19b616" "a62f0662e6aa7b05d0b4493a8e245ab31492765561b08192df61c9d1c7e1ddee" "c614d2423075491e6b7f38a4b7ea1c68f31764b9b815e35c9741e9490119efc0" "1196c0d739fe9fb1f6ca7e6b75ae180ab91dffa73b99eeff57ac4faed72630e8" "604648621aebec024d47c352b8e3411e63bdb384367c3dd2e8db39df81b475f5" "6c35ffc17f8288be4c7866deb7437e8af33cd09930e195738cdfef911ab77274" "a632c5ce9bd5bcdbb7e22bf278d802711074413fd5f681f39f21d340064ff292" "365d9553de0e0d658af60cff7b8f891ca185a2d7ba3fc6d29aadba69f5194c7f" "611e38c2deae6dcda8c5ac9dd903a356c5de5b62477469133c89b2785eb7a14d" "6bde11b304427c7821b72a06a60e8d079b8f7ae10b407d8af37ed5e5d59b1324" "7dbb3a2ea9d4990edbe3fd4f01a9500cfc1587e9da0481fff508d84b1d2b86f2" "227e2c160b0df776257e1411de60a9a181f890cfdf9c1f45535fc83c9b34406b" "7d0de67184bb67d00a45150f050183e8ef8f6277e66ef2da11dbb54b15274cba" "9f3181dc1fabe5d58bbbda8c48ef7ece59b01bed606cfb868dd147e8b36af97c" "a800120841da457aa2f86b98fb9fd8df8ba682cebde033d7dbf8077c1b7d677a" "9f66b8c8ed9387bef299e56b1a0349c74b213a91a8a480e4acbe40ec8a28bfd8" "e0d42a58c84161a0744ceab595370cbe290949968ab62273aed6212df0ea94b4" "01ce486c3a7c8b37cf13f8c95ca4bb3c11413228b35676025fdf239e77019ea1" "03cbd0203ced50377ae2ec63caa9529e91999e1cd50c858bcb64378ab15f2628" "96998f6f11ef9f551b427b8853d947a7857ea5a578c75aa9c4e7c73fe04d10b4" "40f6a7af0dfad67c0d4df2a1dd86175436d79fc69ea61614d668a635c2cd94ab" "7122873f9ac192e4f2cfafe1679fe6b3db658ac64593efe4bc10c52d7573c6c1" "40bc0ac47a9bd5b8db7304f8ef628d71e2798135935eb450483db0dbbfff8b11" "603a9c7f3ca3253cb68584cb26c408afcf4e674d7db86badcfe649dd3c538656" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "38ba6a938d67a452aeb1dada9d7cdeca4d9f18114e9fc8ed2b972573138d4664" "d8f76414f8f2dcb045a37eb155bfaa2e1d17b6573ed43fb1d18b936febc7bbc2" "e003c6adea3ddbbe092c6e2db2e48a1deed2fd06c89534c886bad99fd15e0fc1" "cb978b7187ea7ac2a3e6bb614d24988301cb5c2c9d1f930dce117792b21ea135" "667e296942c561382fe0a8584c26be0fe7a80416270c3beede8c6d69f2f77ccc" "ccad63153802ee95d74ddb8e11a231702399a8765e6f2a056e3051a4861fad9b" "83a27e12cbf3332d5502a953659b768d222a5be4518d52eda046386a66446c19" "8e5dd88c42089566d5f8e1a23d3017c213eeccd94a7b9e1a58a2dc3e08cb26d5" "685a7460fdc4b8c38796234d3a96b3aacbe4fba739fb33b5d6d149051ce74a58" "94ba29363bfb7e06105f68d72b268f85981f7fba2ddef89331660033101eb5e5" "a776135e3d68ebb9c5033799a86290e2243e352f5b8fe6b3b96fbf80c65acd0c" "05c3bc4eb1219953a4f182e10de1f7466d28987f48d647c01f1f0037ff35ab9a" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "4e262566c3d57706c70e403d440146a5440de056dfaeb3062f004da1711d83fc" "64581032564feda2b5f2cf389018b4b9906d98293d84d84142d90d7986032d33" "46fd293ff6e2f6b74a5edf1063c32f2a758ec24a5f63d13b07a20255c074d399" "1a85b8ade3d7cf76897b338ff3b20409cb5a5fbed4e45c6f38c98eee7b025ad4" "987b709680284a5858d5fe7e4e428463a20dfabe0a6f2a6146b3b8c7c529f08b" "e9776d12e4ccb722a2a732c6e80423331bcb93f02e089ba2a4b02e85de1cf00e" "58c6711a3b568437bab07a30385d34aacf64156cc5137ea20e799984f4227265" "3cd28471e80be3bd2657ca3f03fbb2884ab669662271794360866ab60b6cb6e6" "a774c5551bc56d7a9c362dca4d73a374582caedb110c201a09b410c0ebbb5e70" "1011be33e9843afd22d8d26b031fbbb59036b1ce537d0b250347c19e1bd959d0" "7d4d00a2c2a4bba551fcab9bfd9186abe5bfa986080947c2b99ef0b4081cb2a6" "1989847d22966b1403bab8c674354b4a2adf6e03e0ffebe097a6bd8a32be1e19" "90b5269aefee2c5f4029a6a039fb53803725af6f5c96036dee5dc029ff4dff60" "e26780280b5248eb9b2d02a237d9941956fc94972443b0f7aeec12b5c15db9f3" "3b819bba57a676edf6e4881bd38c777f96d1aa3b3b5bc21d8266fa5b0d0f1ebf" "0ebe0307942b6e159ab794f90a074935a18c3c688b526a2035d14db1214cf69c" "9bcb8ee9ea34ec21272bb6a2044016902ad18646bd09fdd65abae1264d258d89" "c7359bd375132044fe993562dfa736ae79efc620f68bab36bd686430c980df1c" "ce79400f46bd76bebeba655465f9eadf60c477bd671cbcd091fe871d58002a88" "4f66410c3d3434129e230eaab99f9319bd5871623689fb56713e38255eb16ddc" "53c542b560d232436e14619d058f81434d6bbcdc42e00a4db53d2667d841702e" "bf648fd77561aae6722f3d53965a9eb29b08658ed045207fe32ffed90433eb52" "cfd79d66fe6b142b570048ed9a28cd2c71876f824d76e1d9f2da0f3353062f3f" "60e97fc4cdb64c43cab637cd0027e09cf27939fe799a1889a30cfedd6f2e7f8e" "0058b7d3e399b6f7681b7e44496ea835e635b1501223797bad7dd5f5d55bb450" "3a727bdc09a7a141e58925258b6e873c65ccf393b2240c51553098ca93957723" "756597b162f1be60a12dbd52bab71d40d6a2845a3e3c2584c6573ee9c332a66e" "6fe6ab4abe97a4f13533e47ae59fbba7f2919583f9162b440dd06707b01f7794" "f07583bdbcca020adecb151868c33820dfe3ad5076ca96f6d51b1da3f0db7105" "0f0adcd1352b15a622afd48fcff8232169aac4b5966841e506f815f81dac44ea" "2c73700ef9c2c3aacaf4b65a7751b8627b95a1fd8cebed8aa199f2afb089a85f" "fd7ef8af44dd5f240e4e65b8a4eecbc37a07c7896d729a75ba036a59f82cfa58" "2cc9ecf74dd307cdf856a2f47f6149583d6cca9616a0f4ecc058bafa57e4ffa3" "f2355ec455645cd4a4b8f8ac8bcb96c50bc8f383634e59307d8bc651143f6be4" "6e03b7f86fcca5ce4e63cda5cd0da592973e30b5c5edf198eddf51db7a12b832" "ef36e983fa01515298c017d0902524862ec7d9b00c28922d6da093485821e1ba" "a3821772b5051fa49cf567af79cc4dabfcfd37a1b9236492ae4724a77f42d70d" "f479660a3c4e3134a840113aeb092e50382779220a0d23017ac66481682fcdcb" "0ff3aeed353697992d100ddf8a94d065a58ffbde5a40afefa605f211757c8ab0" "1faffcddc50d5dc7d334f2817dd6f159ef1820be3aad303eb7f74006531afdff" "70b9e0d0b857d6497c6623bb360a3a7f915251c4a6233c30b65f9005eb9f4256" "6981a905808c6137dc3a3b089b9393406d2cbddde1d9336bb9d372cbc204d592" "c3806e9426f97f54eccd51bb90c1fabb9205bf359d9ab23311638e1a68aae472" "09feeb867d1ca5c1a33050d857ad6a5d62ad888f4b9136ec42002d6cdf310235" "caa9a86ff9b85f733b424f520ec6ecff3499a36f20eb8d40e3096dbbe1884069" "0c5204945ca5cdf119390fe7f0b375e8d921e92076b416f6615bbe1bd5d80c88" "39a854967792547c704cbff8ad4f97429f77dfcf7b3b4d2a62679ecd34b608da" "47e37fa090129214330d13a68549d5c86ccc2c41f4979cb4be130ff945a9859a" "8e997c790c6b22c091edb8a866f545857eaae227a0c41df402711f6ebc70326c" "8016855a07f289a6b2deb248e192633dca0165f07ee5d51f9ba982ec2c36797d" "0f0e3af1ec61d04ff92f238b165dbc6d2a7b4ade7ed9812b4ce6b075e08f49fe" "adbe7ba38c551281f21d760de0840cab0e1259964075a7e46cc2b9fdea4b82d6" "b42cf9ee9e59c3aec585fff1ce35acf50259d8b59f3047e57df0fa38516aa335" "57d7e8b7b7e0a22dc07357f0c30d18b33ffcbb7bcd9013ab2c9f70748cfa4838" "bb6b64bfb2f63efed8dea1ca03691c07c851a8be6f21675fe4909289d68975d9" "53e29ea3d0251198924328fd943d6ead860e9f47af8d22f0b764d11168455a8e" "e16a771a13a202ee6e276d06098bc77f008b73bbac4d526f160faa2d76c1dd0e" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(dante-debug nil)
 '(dante-repl-command-line nil)
 '(dante-repl-command-line-methods-alist
   (quote
    ((styx .
           #[257 "\300\301\302#\207"
                 [dante-repl-by-file
                  ("styx.yaml")
                  ("styx" "repl" dante-target)]
                 5 "

(fn ROOT)"])
     (nix .
          #[257 "\300\301\302#\207"
                [dante-repl-by-file
                 ("shell.nix" "default.nix")
                 ("nix-shell" "-I"
                  (substitute-in-file-name "nixpkgs=$HOME/.nix-defexpr/channels/nixpkgs/")
                  "--run"
                  (if dante-target
                      (concat "cabal repl " dante-target)
                    "cabal repl"))]
                5 "

(fn ROOT)"])
     (stack .
            #[257 "\300\301\302#\207"
                  [dante-repl-by-file
                   ("stack.yaml")
                   ("stack" "repl" dante-target)]
                  5 "

(fn ROOT)"])
     (mafia .
            #[257 "\300\301\302#\207"
                  [dante-repl-by-file
                   ("mafia")
                   ("mafia" "repl" dante-target)]
                  5 "

(fn ROOT)"])
     (new-build .
                #[257 "\300\301\302#\204 \303\304!\205 \305\207"
                      [directory-files nil ".*\\.cabal$" file-exists-p "cabal.project"
                                       ("cabal" "new-repl" dante-target)]
                      5 "

(fn ROOT)"])
     (bare .
           #[257 "\300\207"
                 [("cabal" "repl" dante-target)]
                 2 "

(fn _)"]))))
 '(delete-selection-mode t)
 '(display-battery-mode t)
 '(display-time-day-and-date nil)
 '(display-time-default-load-average nil)
 '(display-time-mode t)
 '(doc-view-continuous t)
 '(ecb-layout-name "left2")
 '(ecb-options-version "2.40")
 '(ecb-source-path (quote (("/" "/") ("/Users/matt/Dropbox" "dropbox"))))
 '(ecb-tip-of-the-day nil)
 '(eclim-eclipse-dirs (quote ("/Applications/eclipse")))
 '(eclim-executable "/Applications/eclipse/eclim")
 '(ede-project-directories (quote ("/Users/matt/Dropbox/cse30/cse30-pa3")))
 '(expand-region-guess-python-mode nil)
 '(fancy-battery-mode nil)
 '(fancy-battery-show-percentage t)
 '(fci-rule-color "#5c5e5e" t)
 '(fci-rule-width 5)
 '(flycheck-command-wrapper-function
(function
 (lambda
   (command)
   (if
       (numberp
        (position major-mode
                  (quote
                   (haskell-mode literate-haskell-mode))))
       (apply
        (quote nix-shell-command)
        (nix-current-sandbox)
        command))
   command)))
 '(flycheck-completion-system (quote ido) t)
 '(flycheck-display-errors-delay 0)
 '(flycheck-display-errors-function (quote flycheck-display-error-messages))
'(flycheck-executable-find
(function
 (lambda
   (cmd)
   (nix-executable-find
    (nix-current-sandbox)
    cmd))))
 '(flycheck-haskell-hdevtools-executable "~/Library/Haskell/bin/hdevtools")
 '(flycheck-haskell-liquid-executable "~/.local/bin//liquid")
 '(flycheck-highlighting-mode (quote sexps))
 '(flyspell-issue-message-flag t)
 '(geiser-active-implementations (quote (racket scheme)))
 '(geiser-default-implementation (quote racket))
'(geiser-implementations-alist
(quote
 (((regexp "\\.scm$")
   scheme)
  ((regexp "\\.ss$")
   scheme)
  ((regexp "\\.rkt$")
   racket))))
 '(geiser-mode-auto-p nil)
 '(geiser-mode-autodoc-p nil)
 '(haskell-font-lock-symbols nil)
'(haskell-mode-hook
(quote
 (#[nil "\301\211\207"
        [indent-tabs-mode nil]
        2]
  interactive-haskell-mode matt/functional-programming
  #[nil "\300\301!\207"
        [electric-indent-mode -1]
        2]
  haskell-indentation-mode)))
'(haskell-process-args-ghci
(quote
 ("ghci" "--with-ghc" "intero" "--no-load" "--no-build")))
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-path-cabal "cabal configure && cabal")
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-type (quote cabal-repl))
 '(haskell-process-use-ghci t)
'(haskell-process-wrapper-function
(function
 (lambda
   (args)
   (apply
    (quote nix-shell-command)
    (nix-current-sandbox)
    args))))
 '(haskell-program-name "stack ghci \"+.\"")
 '(helm-ag-base-command "ag --nocolor --nogroup --ignore-case")
 '(helm-ag-command-option "--skip-vcs-ignores")
 '(helm-ag-insert-at-point (quote symbol))
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
'(highlight-symbol-colors
(--map
 (solarized-color-blend it "#fdf6e3" 0.25)
 (quote
  ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#586e75")
'(highlight-tail-colors
(quote
 (("#eee8d5" . 0)
  ("#B4C342" . 20)
  ("#69CABF" . 30)
  ("#69B7F0" . 50)
  ("#DEB542" . 60)
  ("#F2804F" . 70)
  ("#F771AC" . 85)
  ("#eee8d5" . 100))))
'(hl-bg-colors
(quote
 ("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00")))
'(hl-fg-colors
(quote
 ("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36")))
 '(hl-sexp-background-color "#1c1f26")
 '(idris-interpreter-path "idris" t)
 '(indent-tabs-mode t)
 '(inhibit-startup-echo-area-message "")
 '(initial-scratch-message "")
 '(jdee-db-active-breakpoint-face-colors (cons "#0d0d0d" "#41728e"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#0d0d0d" "#b5bd68"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#0d0d0d" "#5a5b5a"))
 '(liquid-tip-mode nil t)
 '(magit-diff-use-overlays nil)
 '(magit-use-overlays nil)
'(neo-hidden-regexp-list
(quote
 ("^\\." "\\.pyc$" "~$" "^#.*#$" "\\.elc$" "\\.class$")))
'(notmuch-search-line-faces
(quote
 (("unread" :foreground "#aeee00")
  ("flagged" :foreground "#0a9dff")
  ("deleted" :foreground "#ff2c4b" :bold t))))
'(nrepl-message-colors
(quote
 ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(olivetti-body-width 100)
 '(olivetti-minimum-body-width 40)
 '(org-ellipsis " \357\204\207 ")
 '(org-fontify-done-headline t)
 '(org-fontify-quote-and-verse-blocks t)
 '(org-fontify-whole-heading-line t)
 '(org-indent-mode-turns-on-hiding-stars nil)
 '(org-startup-indented t)
 '(org-startup-truncated nil)
'(package-selected-packages
(quote
 (multishell purescript-mode rotate company-lsp lsp-ui basic-theme minimal-theme psc-ide xterm-color treemacs-evil treemacs treemacs-projectile helm-projectile helm-hayoo hayoo fixmee nhexl-mode anzu cmm-mode helm-nixos-options nixos-options nord-theme badwolf-theme define-word typing-game speed-type captain ace-jump-buffer ace-jump-helm-line ace-jump-mode ace-window avy-flycheck avy-menu avy flycheck-haskell-mode dante flycheck-purescript nix-sandbox nix-buffer nix-mode nlinum doom-themes dr-racket-like-unicode helm-hoogle helm-idris lsp-haskell lsp-mode nodejs-repl ecb olivetti intero yaml-mode wc-mode use-package tuareg smartparens sicp shm seti-theme racket-mode pandoc-mode org-journal nginx-mode monokai-theme moe-theme menu-bar+ js2-mode iedit icicles helm-ls-git helm-flyspell helm-company haml-mode git-auto-commit-mode git-annex gist fsharp-mode flycheck-pos-tip flycheck-hdevtools feature-mode expand-region exec-path-from-shell emamux elpy dot-mode django-mode dash-functional zenburn-theme web-mode visual-fill-column spacemacs-theme solarized-theme slime python-mode projectile neotree markdown-mode magit helm-ag helm go-mode furl flycheck swiper find-file-in-project ensime elm-mode ein company-math yasnippet company cider auto-complete docker-tramp scala-outline-popup git-commit-mode flymake-racket clojure-test-mode scala-mode2 znc writeroom-mode writegood-mode wolfram-mode wc-goal-mode w3 utop top-mode tabbar sublime-themes stripe-buffer stack-mode spotify spaces spacegray-theme soft-stone-theme sml-mode smart-mode-line shell-command scheme-complete scf-mode rainbow-delimiters quack psci protobuf-mode paredit osx-plist org-pandoc nose multi-web-mode multi-term memory-usage marmalade load-theme-buffer-local lex latex-preview-pane latex-pretty-symbols keyfreq jtags json-mode jedi-direx impatient-mode image+ igrep idomenu ido-ubiquitous ido-load-library ido-hacks ido-complete-space-or-hyphen ido-at-point hy-mode hippie-expand-slime helm-ls-svn helm-ls-hg helm-gitlab helm-gitignore helm-git helm-ghc helm-flycheck helm-ag-r haskell-emacs-text haskell-emacs-base graphviz-dot-mode gitconfig git-blame git fuzzy fringe-helper framesize fold-dwim-org flyparens flymake-python-pyflakes flymake-hlint flymake-haskell-multi flylisp flycheck-ocaml flycheck-liquidhs flycheck-haskell flycheck-ghcmod flycheck-elm flycheck-color-mode-line flx-ido fill-column-indicator elscreen electric-case display-theme dirtree dired-details+ dired+ ctags-update ctags company-web company-ghci company-ghc company-coq company-cabal company-c-headers company-auctex color-theme coffee-mode cfengine-code-style button-lock bison-mode autopair auto-complete-auctex auctex-lua auctex-latexmk anything-git-grep anything-git-files anything-exuberant-ctags ag adjust-parens ac-slime ac-math ac-ispell ac-haskell-process ac-geiser ac-etags ac-cider ac-c-headers)))
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(proof-electric-terminator-enable nil)
 '(proof-three-window-mode-policy (quote hybrid))
 '(proof-toolbar-enable nil)
 '(purescript-mode-hook (quote (turn-on-purescript-indent)) t)
 '(quack-default-program "mit-scheme")
 '(quack-fontify-style (quote plt))
 '(quack-pretty-lambda-p t)
'(quack-programs
(quote
 ("mit-scheme-x86-64" "bigloo" "csi" "csi -hygienic" "gosh" "gracket" "gsi" "gsi ~~/syntax-case.scm -" "guile" "kawa" "mit-scheme" "racket" "racket -il typed/racket" "rs" "scheme" "scheme48" "scsh" "sisc" "stklos" "sxi")))
 '(racket-program "/Applications/Racket/bin/racket" t)
 '(reftex-plug-into-AUCTeX t t)
 '(safe-local-variable-values (quote ((buffer-file-coding-system . utf-8-unix))))
 '(scheme-mit-dialect t)
 '(send-mail-function (quote mailclient-send-it))
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(sml/battery-format "~%p")
 '(speedbar-default-position (quote left))
 '(sr-speedbar-max-width 50)
 '(tab-width 4)
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(undo-tree-visualizer-timestamps t)
 '(vc-annotate-background "#0d0d0d")
'(vc-annotate-color-map
(list
 (cons 20 "#b5bd68")
 (cons 40 "#c8c06c")
 (cons 60 "#dcc370")
 (cons 80 "#f0c674")
 (cons 100 "#eab56d")
 (cons 120 "#e3a366")
 (cons 140 "#de935f")
 (cons 160 "#d79e84")
 (cons 180 "#d0a9a9")
 (cons 200 "#c9b4cf")
 (cons 220 "#ca9aac")
 (cons 240 "#cb8089")
 (cons 260 "#cc6666")
 (cons 280 "#af6363")
 (cons 300 "#936060")
 (cons 320 "#765d5d")
 (cons 340 "#5c5e5e")
 (cons 360 "#5c5e5e")))
 '(vc-annotate-very-old-color nil)
'(weechat-color-list
(quote
 (unspecified "#fdf6e3" "#eee8d5" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#657b83" "#839496")))
'(xterm-color-names
["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
'(xterm-color-names-bright
["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"])
'(znc-servers
(quote
 (("162.248.141.245" 1701 t
   ((matt-znc "mach" "Z2fqfcxgPdJWn8Y"))))) t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(haskell-default-face ((t (:inherit default))))
 '(haskell-keyword-face ((t (:inherit font-lock-keyword-face))))
 '(term ((t (:inherit default))))
 '(term-color-black ((t (:foreground "#3F3F3F" :background "#2B2B2B"))))
 '(term-color-blue ((t (:foreground "#7CB8BB" :background "#4C7073"))))
 '(term-color-cyan ((t (:foreground "#93E0E3" :background "#8CD0D3"))))
 '(term-color-green ((t (:foreground "#7F9F7F" :background "#9FC59F"))))
 '(term-color-magenta ((t (:foreground "#DC8CC3" :background "#CC9393"))))
 '(term-color-red ((t (:foreground "#AC7373" :background "#8C5353"))))
 '(term-color-white ((t (:foreground "#DCDCCC" :background "#656555"))))
 '(term-color-yellow ((t (:foreground "#DFAF8F" :background "#9FC59F"))))
 '(term-default-bg-color ((t (:inherit term-color-black))))
 '(term-default-fg-color ((t (:inherit term-color-white)))))
