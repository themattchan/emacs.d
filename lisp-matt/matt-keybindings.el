;;; matt-keybindings.el --- Keybindings.

;;; Copyright (c) 2013-2015 Matthew Chan
;;; Author: Matthew Chan <matt@parametri.city>
;;; URL: http://github.com/themattchan/emacs.d

;; Package-Requires: ((bind-key "1.0") (diminish "0.44") (use-package "2.1"))

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

;; WARNING:
;; uses bind-key, provided by use-package. Make sure you require it before loading!
;; to bind to a mode map, do (bind-key <key> <func> &<map>)
;; likewise, to unbind a key from a map, (unbind-key <key> &<map>)

;;; Code:

;; Fix modifier keys on Mac GUI
;; Carbon Emacs. Assume Control is Caps
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

(bind-key "C-;" 'comment-or-uncomment-region)

(bind-key "M-/" 'hippie-expand) ; replace default expand command
;; backtab is now yasnippet complete snippet
;;(bind-key "<backtab>" 'hippie-expand) ; rebind said command to shift-tab


;; Transpose stuff with M-t
;; Put cursor in between <item1> | <item2>
;; C-t is transpose-char
(unbind-key "M-t") ;; which used to be transpose-words
(bind-key "M-t M-c" 'transpose-char)
(bind-key "M-t M-l" 'transpose-lines)
;; (bind-key "M-t M-w" 'transpose-words)
(bind-key "M-t M-t" 'transpose-words)
(bind-key "M-t M-s" 'transpose-sexps)


;; Alignment
(bind-key "C-x a r" 'align-regexp)
(bind-key "M-{" 'align)


;; Navigation
;; backwards then forwards
;; ----------
;; C-b C-f by char
;; M-b M-f by word
;; C-M-b C-M-f by sexpr
;; C-a C-e by line, laterally
;; C-n C-p by line, vertically
;; M-a M-e by sentence
;; M-[ M-] by paragraph
;; (M-{ and M-} is a pain in the ass)
;; M-v C-v by screen
;; M-< M-> start/end

(bind-key "M-[" 'backward-paragraph)
(bind-key "M-]" 'forward-paragraph)

(bind-key "M-<up>" 'scroll-down-command)
(bind-key "M-<down>" 'scroll-up-command)

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

(bind-key "C-<backspace>" 'kill-word)

;; also alias to C-Y
;; (bind-key "C-y" 'yank)
;; (bind-key "C-Y" 'yank-pop)


(defun yank-pop-forwards (arg)
  "Rotate kill-ring forwards."
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
;; (bind-key "<f1>" 'ag-project)
;; (bind-key "M-<f1>" 'ag)                 ;C-<f1> doesn't work for some reason

;; bind to compile
(bind-key "C-x g" 'compile)

;; (bind-key "C-x C-k" (lambda () (interactive) (kill-buffer (current-buffer))))

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
      (bind-key "s-S-<left>"    'windmove-left)     ; move to left window
      (bind-key "s-S-<right>"   'windmove-right)    ; move to right window
      (bind-key "s-S-<up>"      'windmove-up)       ; move to upper window
      (bind-key "s-S-<down>"    'windmove-down)
      (bind-key "s-<left>"      'previous-buffer)   ; move to prev buffer
      (bind-key "s-<right>"     'next-buffer))      ; move to next buffer

  (progn ;; terminal doesn't like the meta key
    (bind-key "C-c <left>"      'windmove-left)
    (bind-key "C-c <right>"     'windmove-right)
    (bind-key "C-c <up>"        'windmove-up)
    (bind-key "C-c <down>"      'windmove-down)))

;; alternate pane navigation on keyboard, no arrow keys
(progn
  (bind-key "C-s-b"         'windmove-left)     ; move to left window
  (bind-key "C-s-f"         'windmove-right)    ; move to right window
  (bind-key "C-s-p"         'windmove-up)       ; move to upper window
  (bind-key "C-s-n"         'windmove-down)
  (bind-key "C-c b"         'previous-buffer)   ; move to prev buffer
  (bind-key "C-c n"         'next-buffer))      ; move to next buffer

(bind-key "<wheel-left>"     'previous-buffer)   ; move to prev buffer
(bind-key "<wheel-right>"    'next-buffer)

(defun toggle-fullscreen-linux ()
  "Toggle full screen on X11."
  (interactive)
  (when (eq window-system 'x)
    (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth))))

;; TODO: require import globals
(when *is-linux* (bind-key "<f11>" 'toggle-fullscreen-linux))

;; Don't need this anymore, use rectangle-mode
;; cua rectangle selection is good.
;; (cua-selection-mode 1)


;; magit

(bind-key "C-x C-g" 'magit-status)

;; neotree
(bind-key "<f8>" 'neotree-toggle)
(setq neo-theme 'nerd)

(provide 'matt-keybindings)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; matt-keybindings.el ends here
