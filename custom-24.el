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
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(asm-comment-char 33)
 '(battery-mode-line-format " bat:%p% ")
 '(c-block-comment-prefix (quote set-from-style))
 '(c-comment-prefix-regexp (quote set-from-style))
 '(c-doc-comment-style
   (quote
    ((c-mode . gtkdoc)
     (java-mode . javadoc)
     (pike-mode . autodoc))))
 '(comment-multi-line t)
 '(comment-style (quote multi-line))
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes
   (quote
    ("a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "4e262566c3d57706c70e403d440146a5440de056dfaeb3062f004da1711d83fc" "64581032564feda2b5f2cf389018b4b9906d98293d84d84142d90d7986032d33" "46fd293ff6e2f6b74a5edf1063c32f2a758ec24a5f63d13b07a20255c074d399" "1a85b8ade3d7cf76897b338ff3b20409cb5a5fbed4e45c6f38c98eee7b025ad4" "987b709680284a5858d5fe7e4e428463a20dfabe0a6f2a6146b3b8c7c529f08b" "e9776d12e4ccb722a2a732c6e80423331bcb93f02e089ba2a4b02e85de1cf00e" "58c6711a3b568437bab07a30385d34aacf64156cc5137ea20e799984f4227265" "3cd28471e80be3bd2657ca3f03fbb2884ab669662271794360866ab60b6cb6e6" "a774c5551bc56d7a9c362dca4d73a374582caedb110c201a09b410c0ebbb5e70" "1011be33e9843afd22d8d26b031fbbb59036b1ce537d0b250347c19e1bd959d0" "7d4d00a2c2a4bba551fcab9bfd9186abe5bfa986080947c2b99ef0b4081cb2a6" "1989847d22966b1403bab8c674354b4a2adf6e03e0ffebe097a6bd8a32be1e19" "90b5269aefee2c5f4029a6a039fb53803725af6f5c96036dee5dc029ff4dff60" "e26780280b5248eb9b2d02a237d9941956fc94972443b0f7aeec12b5c15db9f3" "3b819bba57a676edf6e4881bd38c777f96d1aa3b3b5bc21d8266fa5b0d0f1ebf" "0ebe0307942b6e159ab794f90a074935a18c3c688b526a2035d14db1214cf69c" "9bcb8ee9ea34ec21272bb6a2044016902ad18646bd09fdd65abae1264d258d89" "c7359bd375132044fe993562dfa736ae79efc620f68bab36bd686430c980df1c" "ce79400f46bd76bebeba655465f9eadf60c477bd671cbcd091fe871d58002a88" "4f66410c3d3434129e230eaab99f9319bd5871623689fb56713e38255eb16ddc" "53c542b560d232436e14619d058f81434d6bbcdc42e00a4db53d2667d841702e" "bf648fd77561aae6722f3d53965a9eb29b08658ed045207fe32ffed90433eb52" "cfd79d66fe6b142b570048ed9a28cd2c71876f824d76e1d9f2da0f3353062f3f" "60e97fc4cdb64c43cab637cd0027e09cf27939fe799a1889a30cfedd6f2e7f8e" "0058b7d3e399b6f7681b7e44496ea835e635b1501223797bad7dd5f5d55bb450" "3a727bdc09a7a141e58925258b6e873c65ccf393b2240c51553098ca93957723" "756597b162f1be60a12dbd52bab71d40d6a2845a3e3c2584c6573ee9c332a66e" "6fe6ab4abe97a4f13533e47ae59fbba7f2919583f9162b440dd06707b01f7794" "f07583bdbcca020adecb151868c33820dfe3ad5076ca96f6d51b1da3f0db7105" "0f0adcd1352b15a622afd48fcff8232169aac4b5966841e506f815f81dac44ea" "2c73700ef9c2c3aacaf4b65a7751b8627b95a1fd8cebed8aa199f2afb089a85f" "fd7ef8af44dd5f240e4e65b8a4eecbc37a07c7896d729a75ba036a59f82cfa58" "2cc9ecf74dd307cdf856a2f47f6149583d6cca9616a0f4ecc058bafa57e4ffa3" "f2355ec455645cd4a4b8f8ac8bcb96c50bc8f383634e59307d8bc651143f6be4" "6e03b7f86fcca5ce4e63cda5cd0da592973e30b5c5edf198eddf51db7a12b832" "ef36e983fa01515298c017d0902524862ec7d9b00c28922d6da093485821e1ba" "a3821772b5051fa49cf567af79cc4dabfcfd37a1b9236492ae4724a77f42d70d" "f479660a3c4e3134a840113aeb092e50382779220a0d23017ac66481682fcdcb" "0ff3aeed353697992d100ddf8a94d065a58ffbde5a40afefa605f211757c8ab0" "1faffcddc50d5dc7d334f2817dd6f159ef1820be3aad303eb7f74006531afdff" "70b9e0d0b857d6497c6623bb360a3a7f915251c4a6233c30b65f9005eb9f4256" "6981a905808c6137dc3a3b089b9393406d2cbddde1d9336bb9d372cbc204d592" "c3806e9426f97f54eccd51bb90c1fabb9205bf359d9ab23311638e1a68aae472" "09feeb867d1ca5c1a33050d857ad6a5d62ad888f4b9136ec42002d6cdf310235" "caa9a86ff9b85f733b424f520ec6ecff3499a36f20eb8d40e3096dbbe1884069" "0c5204945ca5cdf119390fe7f0b375e8d921e92076b416f6615bbe1bd5d80c88" "39a854967792547c704cbff8ad4f97429f77dfcf7b3b4d2a62679ecd34b608da" "47e37fa090129214330d13a68549d5c86ccc2c41f4979cb4be130ff945a9859a" "8e997c790c6b22c091edb8a866f545857eaae227a0c41df402711f6ebc70326c" "8016855a07f289a6b2deb248e192633dca0165f07ee5d51f9ba982ec2c36797d" "0f0e3af1ec61d04ff92f238b165dbc6d2a7b4ade7ed9812b4ce6b075e08f49fe" "adbe7ba38c551281f21d760de0840cab0e1259964075a7e46cc2b9fdea4b82d6" "b42cf9ee9e59c3aec585fff1ce35acf50259d8b59f3047e57df0fa38516aa335" "57d7e8b7b7e0a22dc07357f0c30d18b33ffcbb7bcd9013ab2c9f70748cfa4838" "bb6b64bfb2f63efed8dea1ca03691c07c851a8be6f21675fe4909289d68975d9" "53e29ea3d0251198924328fd943d6ead860e9f47af8d22f0b764d11168455a8e" "e16a771a13a202ee6e276d06098bc77f008b73bbac4d526f160faa2d76c1dd0e" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(delete-selection-mode t)
 '(display-battery-mode t)
 '(display-time-day-and-date nil)
 '(display-time-default-load-average nil)
 '(display-time-mode t)
 '(doc-view-continuous t)
 '(ecb-layout-name "left2")
 '(ecb-options-version "2.40")
 '(ecb-source-path (quote (("/" "/"))))
 '(ecb-tip-of-the-day nil)
 '(ede-project-directories (quote ("/Users/matt/Dropbox/cse30/cse30-pa3")))
 '(fancy-battery-mode nil)
 '(fancy-battery-show-percentage t)
 '(fci-rule-color "#343d46")
 '(fci-rule-width 5)
 '(flycheck-completion-system (quote ido))
 '(flycheck-display-errors-delay 0)
 '(flycheck-display-errors-function (quote flycheck-display-error-messages))
 '(flycheck-haskell-hdevtools-executable "~/Library/Haskell/bin/hdevtools")
 '(flycheck-haskell-liquid-executable "~/Library/Haskell/bin/liquid")
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
 '(haskell-font-lock-symbols t)
 '(haskell-mode-hook
   (quote
    (#[nil "\301\211\207"
           [indent-tabs-mode nil]
           2]
     inf-haskell-mode interactive-haskell-mode
     #[nil "\300\301!\207"
           [liquid-tip-init ascii]
           2]
     #[nil "\300\301!\207"
           [flycheck-select-checker haskell-hdevtools]
           2]
     matt/functional-programming
     #[nil "\300\301!\207"
           [electric-indent-mode -1]
           2]
     haskell-indentation-mode)))
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-type (quote ghci))
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
 '(indent-tabs-mode t)
 '(inhibit-startup-echo-area-message "")
 '(initial-scratch-message "")
 '(magit-diff-use-overlays nil)
 '(magit-use-overlays nil)
 '(org-startup-indented t)
 '(org-startup-truncated nil)
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(quack-default-program "mit-scheme")
 '(quack-fontify-style (quote plt))
 '(quack-pretty-lambda-p t)
 '(quack-programs
   (quote
    ("mit-scheme-x86-64" "bigloo" "csi" "csi -hygienic" "gosh" "gracket" "gsi" "gsi ~~/syntax-case.scm -" "guile" "kawa" "mit-scheme" "racket" "racket -il typed/racket" "rs" "scheme" "scheme48" "scsh" "sisc" "stklos" "sxi")))
 '(reftex-plug-into-AUCTeX t)
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
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#bf616a")
     (40 . "#DCA432")
     (60 . "#ebcb8b")
     (80 . "#B4EB89")
     (100 . "#89EBCA")
     (120 . "#89AAEB")
     (140 . "#C189EB")
     (160 . "#bf616a")
     (180 . "#DCA432")
     (200 . "#ebcb8b")
     (220 . "#B4EB89")
     (240 . "#89EBCA")
     (260 . "#89AAEB")
     (280 . "#C189EB")
     (300 . "#bf616a")
     (320 . "#DCA432")
     (340 . "#ebcb8b")
     (360 . "#B4EB89"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#fdf6e3" "#eee8d5" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#657b83" "#839496"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-error ((t (:background "pink" :foreground "red" :underline (:color "Red1" :style wave)))))
 '(haskell-default-face ((t (:inherit default))))
 '(haskell-keyword-face ((t (:inherit font-lock-keyword-face))))
 '(hl-line ((t (:inherit highlight :box nil)))))
