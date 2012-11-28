(custom-set-variables
   '(evil-shift-width 2)  ;set shift-width
   ;If t (the default), then repeating a command with . may change the position of the cursor. If nil, then the original position is preserved.
   '(evil-repeat-move-cursor t)
   ;If t, then f, F, t and T may skip over newlines to find a character. If nil (the default), then they are restricted to the current line.
   '(evil-find-skip-newlines t)
   ;If t (the default), then the cursor moves backwards when exiting Insert state. If nil, then the cursor does not move.
   '(evil-move-cursor-back nil)
   ;If t, then a change-based action like cw may be undone in several steps. If nil (the default), then it is undone in one step.
   '(evil-want-fine-undo t)
   ;If t (the default), then / and ? use regular expressions for searching. If nil, they use plain text.
   '(evil-regexp-search t)
   ;If t (the default), then / and ? wrap the search around the buffer. If nil, then they stop at buffer boundaries.
   '(evil-search-wrap t)
   ;The number of seconds to flash search matches when pressing n and N.  [Variable]
   '(evil-flash-delay 1)
   ;If t (the default), then C-i jumps forwards in the jump list. If nil, then C-i inserts a tab.  
   '(evil-want-C-i-jump t)
   ;If t, then C-u scrolls the buffer. If nil (the default), then C-u begins a numeric prefix
   '(evil-want-C-u-scroll nil)
   '(evil-leader/leader ",")
   '(evil-leader/in-all-states t)
 )
(push 'evil-leader el-get-packages)
(push 'evil-numbers el-get-packages)
(push 'evil el-get-packages)


 ;(require 'smex)
(defun tahti-after-evil ()
  (undo-tree-mode 1)
  (message "Evil loading")
  ;Indicator settings
     (setq evil-normal-state-tag (propertize "N" 'face '((:background "white"   :foreground "black")))
        evil-insert-state-tag    (propertize "I" 'face '((:background "yellow"  :foreground "black")))
        evil-visual-state-tag    (propertize "V" 'face '((:background "purple"  :foreground "black")))
        evil-replace-state-tag   (propertize "R" 'face '((:background "red")))
        evil-operator-state-tag  (propertize "O" 'face '((:background "#FFDD000":foreground "black")))
        evil-motion-state-tag    (propertize "M" 'face '((:background "blue")))
        evil-emacs-state-tag     (propertize "E" 'face '((:background "green"    :foreground "black")))
  )
  ;Cursor settings
    ;t        use the cursor specified for the frame
    ;nil      don't display a cursor
    ;box      display a filled box cursor
    ;hollow   display a hollow box cursor
    ;bar      display a vertical bar cursor with default width
    ;hbar     display a horizontal bar cursor with default height
    ;(bar . WIDTH)    display a vertical bar cursor with width WIDTH
    ;(hbar . HEIGHT)  display a horizontal bar cursor with height HEIGHT
    ;If the state does not specify a cursor, ‘evil-default-cursor’ is used.
    (setq evil-default-cursor        '("white" box)) ;The default cursor.
    (setq evil-normal-state-cursor   '("white" box))
    (setq evil-insert-state-cursor   '("yellow" bar))
    (setq evil-visual-state-cursor   '("purple" box))
    (setq evil-replace-state-cursor  '("red" box))
    (setq evil-operator-state-cursor '("#FFDD00" hollow))
    (setq evil-motion-state-cursor   '("blue" box))
    (setq evil-emacs-state-cursor    '("green" box))
  (evil-mode 1)
)
(provide 'tahti-evil)
;;;; tahti-evil.el ends here
