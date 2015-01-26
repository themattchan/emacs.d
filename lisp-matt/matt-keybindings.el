;;==============================================================================
;;  KEYBINDINGS
;;==============================================================================

;; WARNING:
;; uses bind-key, provided by use-package. Make sure you require it before loading!
;; to bind to a mode map, do (bind-key <key> <func> &<map>)
;; likewise, to unbind a key from a map, (unbind-key <key> &<map>)

;; fix modifier keys on Mac GUI
;; carbon emacs. Assume control is caps
(setq
 ns-command-modifier 'meta              ; L command -> M
 ns-option-modifier 'super              ; L option -> super
 ;; ns-control-modifier 'super          ; L control -> super
 ;; ns-function-modifier 'hyper         ; fn -> super
 ns-function-modifier 'super

 ;; right hand side modifiers
 ns-right-command-modifier 'super       ; R command -> super
 ns-right-option-modifier 'hyper        ; R option -> hyper
 )

;; type brackets in pairs with Super and right hands's home-row
;; (bind-key "s-j" (lambda () (interactive) (insert "()" (backward-char 1))))
;; (bind-key "s-k" (lambda () (interactive) (insert "[]" (backward-char 1))))
;; (bind-key "s-l" (lambda () (interactive) (insert "{}" (backward-char 1))))

(bind-key "C-;" 'comment-or-uncomment-region)
(bind-key "M-/" 'hippie-expand) ; replace default expand command
(bind-key "<backtab>" 'hippie-expand) ; rebind said command to shift-tab

;; Transpose stuff with M-t
(unbind-key "M-t") ;; which used to be transpose-words
(bind-key "M-t M-l" 'transpose-lines)
;; (bind-key "M-t M-w" 'transpose-words)
(bind-key "M-t M-t" 'transpose-words)
(bind-key "M-t M-s" 'transpose-sexps)

(bind-key "C-x a r" 'align-regexp)
(bind-key "M-{" 'align)

;; Navigation
;; backwards then forwards
;; ----------
;; C-b C-f by char
;; M-b M-f by word
;; C-M-b C-M-f by sexpr
;; C-a C-e by line, laterally
;; C-p C-f by line, vertically
;; M-a M-e by sentence
;; M-[ M-] by paragraph
;; (M-{ and M-} is a pain in the ass)
;; M-v C-v by screen
;; M-< M-> start/end

(bind-key "M-[" 'backward-paragraph)
(bind-key "M-]" 'forward-paragraph)

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
;; (bind-key "C-y" 'yank)
;; (bind-key "C-Y" 'yank-pop)
;; ;; M-Y rotate kill ring forwards
(defun yank-pop-forwards (arg)
      (interactive "p")
      (yank-pop (- arg)))
(bind-key "M-Y" 'yank-pop-forwards)
;; undo/redo
;; C-/ undo, C-? (C-shift-/) redo
;; (bind-key "C-x u" 'undo)
(when (require 'redo nil 'noerror) (bind-key "C-?" 'redo))

;; rebind normal search to use regex
(bind-key "C-s" 'isearch-forward-regexp)
(bind-key "C-r" 'isearch-backward-regexp)

;; access normal isearch with meta key (though kind of useless)
(bind-key "C-M-s" 'isearch-forward)
(bind-key "C-M-r" 'isearch-backward)

;; silver searcher
(bind-key "<f1>" 'ag-project)
(bind-key "M-<f1>" 'ag)                 ;C-<f1> doesn't work for some reason

;; bind to compile
(bind-key "C-x g" 'compile)

;; (bind-key "C-x C-k"
;;              (lambda () (interactive) (kill-buffer (current-buffer))))

;; cycle through split-panes
(bind-key "C-c n"
                (lambda () (interactive) (select-window (next-window))))
(bind-key "C-c p"
                (lambda () (interactive) (select-window (previous-window))))
;; cycle through frames (on the terminal, these fill the screen like tabs)
;; C-x 5 2 make new frame
;; C-x 5 b open buffer in new frame
;; C-x 5 f open file in new frame
;; C-x 5 o cycle to other frame
;; C-x 5 0 close frame

(bind-key "C-c f" 'other-frame)
;; easier split pane navigation
(if (window-system)
    (progn
      (bind-key "M-S-<left>" 'windmove-left)   ; move to left window
      (bind-key "M-S-<right>" 'windmove-right) ; move to right window
      (bind-key "M-S-<up>" 'windmove-up)       ; move to upper window
      (bind-key "M-S-<down>" 'windmove-down)
      (bind-key "s-<left>" 'previous-buffer) ; move to prev buffer
      (bind-key "s-<right>" 'next-buffer))   ; move to next buffer
  (progn                             ;; terminal doesn't like the meta key
    (bind-key "C-c <left>"  'windmove-left)
    (bind-key "C-c <right>" 'windmove-right)
    (bind-key "C-c <up>"    'windmove-up)
    (bind-key "C-c <down>"  'windmove-down)))

(defun toggle-fullscreen-linux ()
  "Toggle full screen on X11"
  (interactive)
  (when (eq window-system 'x)
    (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth))))

(if *is-linux* (bind-key "<f11>" 'toggle-fullscreen-linux))

;; cua rectangle selection is good.
(cua-selection-mode 1)

;; select lines by dragging on linum
(defvar *linum-mdown-line* nil)

(defun line-at-click ()
  (save-excursion
    (let ((click-y (cdr (cdr (mouse-position))))
          (line-move-visual-store line-move-visual))
      (setq line-move-visual t)
      (goto-char (window-start))
      (next-line (1- click-y))
      (setq line-move-visual line-move-visual-store)
      ;; If you are using tabbar substitute the next line with
      ;; (line-number-at-pos))))
      (1+ (line-number-at-pos)))))

(defun md-select-linum ()
  (interactive)
  (goto-line (line-at-click))
  (set-mark (point))
  (setq *linum-mdown-line*
        (line-number-at-pos)))

(defun mu-select-linum ()
  (interactive)
  (when *linum-mdown-line*
    (let (mu-line)
      ;; (goto-line (line-at-click))
      (setq mu-line (line-at-click))
      (goto-line (max *linum-mdown-line* mu-line))
      (set-mark (line-end-position))
      (goto-line (min *linum-mdown-line* mu-line))
      (setq *linum-mdown*
            nil))))

(bind-key "<left-margin> <down-mouse-1>" 'md-select-linum)
(bind-key "<left-margin> <mouse-1>" 'mu-select-linum)
(bind-key "<left-margin> <drag-mouse-1>" 'mu-select-linum)

(provide 'matt-keybindings)
