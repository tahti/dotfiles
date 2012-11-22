;General settings that needs to be set up before evil loads
   ;set shift-width
   (setq evil-shift-width 2)
   ;If t (the default), then repeating a command with . may change the position of the cursor. If nil, then the original position is preserved.
   (setq evil-repeat-move-cursor t)
   ;If t, then f, F, t and T may skip over newlines to find a character. If nil (the default), then they are restricted to the current line.
   (setq evil-find-skip-newlines t)
   ;If t (the default), then the cursor moves backwards when exiting Insert state. If nil, then the cursor does not move.
   (setq evil-move-cursor-back nil)
   ;If t, then a change-based action like cw may be undone in several steps. If nil (the default), then it is undone in one step.
   (setq evil-want-fine-undo t)
   ;If t (the default), then / and ? use regular expressions for searching. If nil, they use plain text.
   (setq evil-regexp-search t)
   ;If t (the default), then / and ? wrap the search around the buffer. If nil, then they stop at buffer boundaries.
   (setq evil-search-wrap t)
   ;The number of seconds to flash search matches when pressing n and N.  [Variable]
   (setq evil-flash-delay 1)
   ;If t (the default), then C-i jumps forwards in the jump list. If nil, then C-i inserts a tab.  
   (setq evil-want-C-i-jump t)
   ;If t, then C-u scrolls the buffer. If nil (the default), then C-u begins a numeric prefix
   (setq evil-want-C-u-scroll nil)

 (setq evil-leader/leader "," evil-leader/in-all-states t)
 (require 'evil-leader)
 (require 'undo-tree)
 (require 'evil-numbers)
 (require 'evil)
 (require 'tahti-util)
 (require 'tahti-func)
 (require 'tahti-windowing)
 
 ;(require 'smex)

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
  (setq evil-default-cursor        '("white" box))
  ;The default cursor.
  (setq evil-normal-state-cursor   '("white" box))
  ;The cursor for Normal state.
  (setq evil-insert-state-cursor   '("yellow" bar))
  ;The cursor for Insert state.
  (setq evil-visual-state-cursor   '("purple" box))
  ;The cursor for Visual state.
  (setq evil-replace-state-cursor  '("red" box))
  ;The cursor for Replace state.
  (setq evil-operator-state-cursor '("#FFDD00" hollow))
  ;The cursor for Operator-Pending state.
  (setq evil-motion-state-cursor   '("blue" (hbar . 7)))
  ;The cursor for Motion state.
  (setq evil-emacs-state-cursor    '("green" box))
  ;The cursor for Emacs state.

;Keymaps
  ;Redefine direction keys:
    ;l -> n
    (define-key evil-motion-state-map "n" 'evil-forward-char)
    (define-key evil-motion-state-map "N" 'evil-window-bottom)
    (define-key evil-motion-state-map (kbd "M-n") 'evil-window-right)
    (define-key evil-motion-state-map (kbd "M-N") 'evil-window-move-far-right)
    ;(define-key evil-motion-state-map "N" 'evil-window-bottom)
    (define-key evil-window-map       "n" 'evil-window-right)
    (define-key evil-window-map       "L" nil)
    (define-key evil-window-map       "N" 'evil-window-move-far-right)
    ;n -> k
    (define-key evil-motion-state-map "k" 'evil-search-next)
    (define-key evil-motion-state-map "K" 'evil-search-previous)
    (define-key evil-window-map       "k" 'delete-window)
    ;k -> c
    (define-key evil-motion-state-map "c" 'evil-previous-line)
    (define-key evil-motion-state-map "C" 'evil-lookup)
    (define-key evil-window-map       "c" 'evil-window-up)
    (define-key evil-motion-state-map (kbd "M-c") 'evil-window-up)
    (define-key evil-motion-state-map (kbd "M-C") 'evil-window-move-very-top)
    (define-key evil-window-map       "C" 'evil-window-move-very-top)
    ;c -> j
    (define-key evil-normal-state-map "c" nil) ;otherwise c is still 'evil-change
    (define-key evil-normal-state-map "C" nil)
    (define-key evil-normal-state-map "j" 'evil-change)
    (define-key evil-normal-state-map "J" 'evil-change-line)
    (define-key evil-window-map       "j" 'evil-window-new)
    ;j -> t
    (define-key evil-motion-state-map "j" nil)
    (define-key evil-normal-state-map "J" nil)
    (define-key evil-motion-state-map "t" 'evil-next-line)
    (define-key evil-normal-state-map "T" 'evil-join)
    (define-key evil-window-map       "t" 'evil-window-down)
    (define-key evil-motion-state-map (kbd "M-t") 'evil-window-down)
    (define-key evil-motion-state-map (kbd "M-T") 'evil-window-move-very-bottom)
    (define-key evil-window-map       "J" nil)
    (define-key evil-window-map       "T" 'evil-window-move-very-bottom)
    ;t -> l
    (define-key evil-motion-state-map       "l" 'evil-find-char-to)
    (define-key evil-motion-state-map       "L" 'evil-find-char-to-backward)
    (define-key evil-outer-text-objects-map "l" 'evil-a-tag)
    (define-key evil-inner-text-objects-map "l" 'evil-inner-tag)
    (define-key evil-window-map             "l" 'evil-window-top-left)


   ;fix enter behaviour
    (evil-declare-key 'motion completion-list-mode-map (kbd "<return>") 'choose-completion)
    (evil-declare-key 'motion completion-list-mode-map (kbd "RET") 'choose-completion)
    (evil-declare-key 'motion browse-kill-ring-mode-map (kbd "<return>") 'browse-kill-ring-insert-and-quit)
    (evil-declare-key 'motion browse-kill-ring-mode-map (kbd "RET") 'browse-kill-ring-insert-and-quit)
    (evil-declare-key 'motion occur-mode-map (kbd "<return>") 'occur-mode-goto-occurrence)
    (evil-declare-key 'motion occur-mode-map (kbd "RET") 'occur-mode-goto-occurrence);
    ;fix ZZ ZQ bindings
    (define-key evil-motion-state-map "ZZ" 'evil-save-modified-and-close)
    (define-key evil-motion-state-map "ZQ" 'evil-quit)
;evil-leader settings
   (defalias 'tahti/file (f-alt 'helm-find-files 'ido-find-file))
   (defalias 'tahti/file-alternate (f-alt 'ido-find-file 'helm-find-files))
   (defalias 'tahti/buffer (f-alt 'tahti/helm-buffers 'ido-switch-buffer))
   (defalias 'tahti/buffer-alternate (f-alt 'ido-switch-buffer 'tahti/helm-buffers))
   ;Keybindings
   (evil-leader/set-key
       "b" 'tahti/buffer
       "B" 'tahti/buffer-alternate
       "e" 'tahti/file
       "E" 'tahti/file-alternate
       "." 'evil-ex
       "o" 'piotr-find-at-alias
       "O" 'piotr-find-helm-at-alias
       "w" 'save-buffer
       "W" 'save-some-buffers
       "k" 'kill-current-buffer
       "K" 'kill-buffer-and-window
       "<" 'piotr-cd-alias
       "d" 'dired-jump
       "D" 'piotr-dired-alias
       ;"h" 'monky-status
       "n" 'split-window-horizontally
       ;"c" 'delete-window
       "g" 'magit-status
       "m" 'compile)

   ;   "N" 'make-frame-command
   ;   "C" 'delete-frame


   ;   "s" 'piotr/switch-file
   ;   ";" 'piotr/end-prog-line

   ;(define-key evil-motion-state-map ":" 'smex-major-mode-commands)
    (define-key evil-motion-state-map (kbd "M-h") 'evil-window-left)
    (define-key evil-motion-state-map (kbd "M-H") 'evil-window-move-far-left)
    (fill-keymap evil-window-map
        "C-h" nil
        "d" 'piotr/window-toggle-dedicate
        ;; Splitting
        "s" 'piotr/smart-split
        "\\" 'split-window-vertically
        "|" 'split-window-horizontally
        "/" 'piotr/multi-split

        ;; Deleting
        "D"   'delete-window
        "C-d" 'delete-window
        "1"   'delete-other-windows

        ;; Sizing
        "RET" 'enlarge-window
        "-"   'shrink-window-horizontally
        "+"   'enlarge-window-horizontally

        ;; Moving
        "<left>"  'evil-window-left
        "<down>"  'evil-window-down
        "<up>"    'evil-window-up
        "<right>" 'evil-window-right

        ;; Swapping
        "M-h"       'swap-with-left
        "M-t"       'swap-with-down
        "M-c"       'swap-with-up
        "M-n"       'swap-with-right
        "S-<left>"  'swap-with-left
        "S-<down>"  'swap-with-down
        "S-<up>"    'swap-with-up
        "S-<right>" 'swap-with-right
        "SPC"       'swap-window

        "g" 'piotr/goto-window

        ;; winner-mode
        "u" 'winner-undo
        "C-r" 'winner-redo
        ;; shadow rotating in evil-window-map
        "C-R" 'winner-redo)

(evil-mode 1)

(provide 'tahti-evil)
