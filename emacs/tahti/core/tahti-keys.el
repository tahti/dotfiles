(require 'tahti-util)
(require 'tahti-helm)
(require 'tahti-windowing)
(require 'tahti-evil)
(require 'tahti-autoloads)
;;;; Keys used by evil =======================================
; we change default motion keys for covninient dvorak navigation: 
; l->n n->k  k->c  c->j  j->t  t->l, h stays h
    (fill-keymap evil-normal-state-map
       "c"    nil ;otherwise c is still 'evil-change
       "C"    nil
       "j"    'evil-change
       "J"    'evil-change-line
       "T"    'evil-join
       "C-,"   evil-leader/map
    )
    (fill-keymap evil-motion-state-map
       "n"    'evil-forward-char
       "N"    'evil-window-bottom
       "M-N"  'evil-window-move-far-right
       "k"    'evil-search-next
       "K"    'evil-search-previous
       "c"    'evil-previous-line
       "C"    'evil-lookup
       "M-C"  'evil-window-move-very-top
       "j"     nil
       "t"    'evil-next-line
       "M-T"  'evil-window-move-very-bottom
       "l"    'evil-find-char-to
       "L"    'evil-find-char-to-backward
       "M-H"  'evil-window-move-far-left
       "ZZ"   'evil-save-modified-and-close ;allow quiting from help files
       "ZQ"   'evil-quit                    ;allow quiting from help files
       evil-leader/leader evil-leader/map   ;make leader work in motion mode
    )
    (define-key evil-outer-text-objects-map "l" 'evil-a-tag)
    (define-key evil-inner-text-objects-map "l" 'evil-inner-tag)

    (fill-keymap evil-window-map
       ;; Movement 
       ;"n"    'evil-window-right
       "N"    'evil-window-move-far-right
       "k"    'delete-window
       "C"    'evil-window-move-very-top
       "j"    'evil-window-new
       "J"    nil
       ;"t"    'evil-window-down
       "T"    'evil-window-move-very-bottom
       "l"    'evil-window-top-left
       "L"    nil

       "C-h"   nil
        "d"   'tahti/window-toggle-dedicate
        ;; Splitting
        "s"   'tahti/smart-split
        "\\"  'split-window-vertically
        "|"   'split-window-horizontally
        "/"   'tahti/multi-split
        "f"   'follow-mode

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
        "H"         'swap-with-left
        "T"         'swap-with-down
        "C"         'swap-with-up
        "N"         'swap-with-right
        "SPC"       'swap-window

        "g" 'tahti/goto-window
        "c"    'compare-windows

        ;; winner-mode
        "u" 'winner-undo
        "C-r" 'winner-redo
        ;; shadow rotating in evil-window-map
        "C-R" 'winner-redo
    )
;; evil leader is set in tahti-evil.el to ","
   (evil-leader/set-key
       "b" 'tahti/buffer
       "B" 'tahti/buffer-alternate
       "e" 'tahti/file
       "E" 'tahti/file-alternate
       "." 'evil-ex
       "o" 'tahti-find-at-alias
       "O" 'tahti-find-helm-at-alias
       "w" 'save-buffer
       "W" 'save-some-buffers
       "k" 'kill-current-buffer
       "K" 'kill-buffer-and-window
       "<" 'tahti-cd-alias
       "d" 'dired-jump
       "D" 'tahti-dired-alias
       ;"h" 'monky-status
       "n" 'split-window-horizontally
       ;"c" 'delete-window
       "g" 'magit-status
       "," 'evil-repeat-find-char-reverse
       "m" 'compile)

   ;   "N" 'make-frame-command
   ;   "C" 'delete-frame


   ;   "s" 'tahti/switch-file
   ;   ";" 'tahti/end-prog-line

   ;fix enter behaviour
    (evil-declare-key 'motion completion-list-mode-map (kbd "<return>") 'choose-completion)
    (evil-declare-key 'motion completion-list-mode-map (kbd "RET") 'choose-completion)
    (evil-declare-key 'motion browse-kill-ring-mode-map (kbd "<return>") 'browse-kill-ring-insert-and-quit)
    (evil-declare-key 'motion browse-kill-ring-mode-map (kbd "RET") 'browse-kill-ring-insert-and-quit)
    (evil-declare-key 'motion occur-mode-map (kbd "<return>") 'occur-mode-goto-occurrence)
    (evil-declare-key 'motion occur-mode-map (kbd "RET") 'occur-mode-goto-occurrence);


;;;; Rest of the keys ========================================
;; use shift + arrow keys to switch between visible buffers
(require 'windmove)
(windmove-default-keybindings)

;(define-key yas-minor-mode-map "\C-c&" nil)

;; A complementary binding to the apropos-command (C-h a)
(define-key 'help-command "A" 'apropos)


(fill-keymap 'global
  "C-x g" 'magit-status
  "C-+"   'text-scale-increase  ;;increase font
  "C--"   'text-scale-decrease  ;;decrease font
  "M-h"   'evil-window-left
  "M-n"   'evil-window-right
  "M-c"   'evil-window-up
  "M-t"   'evil-window-down
  )


(provide 'tahti-keys)
;;;; tahti-keys ends here
