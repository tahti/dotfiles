(require 'tahti-util)
(require 'tahti-helm)
(require 'tahti-evil)
(require 'tahti-windowing)
;(require 'tahti-evil)
;;;; Keys used by evil =======================================
; we change default motion keys for covninient dvorak navigation: 
; l->n n->k  k->c  c->j  j->t  t->l, h stays h
(defun kill-current-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))

(defun tahti-toggle-comment ()
  (interactive)
  (if mark-active (call-interactively 'comment-or-uncomment-region)
                  (comment-or-uncomment-region(line-beginning-position)(line-end-position)))
)


(defun tahti-esc()
  "Functionality for escaping generally. Closes help buffer."
  (interactive)
     (cond
      ;(and (boundp 'some-mode) some-mode)
      ;(evil-visual-state-p) (evil-normal-state)
      ((or (equal 'help-mode major-mode)
           (equal 'apropos-mode major-mode)) 
       (kill-buffer-and-window))
      ((equal 'undo-tree-visualizer-mode major-mode) (undo-tree-visualizer-quit))
      (t (keyboard-escape-quit))))

(defun tahti-evil-keys ()
    ;(fill-keymap evil-visual-state-map
       ;"<escape>" 'tahti-esc ;we do not want previous state but normal state
    ;)
    ;(fill-keymap evil-emacs-state-map
       ;"<escape>" 'evil-normal-state ;we do not want previous state but normal state
    ;)
    (fill-keymap evil-normal-state-map
       "c"    nil ;otherwise c is still 'evil-change
       "C"    nil
       "j"    'evil-change
       "J"    'evil-change-line
       "T"    'evil-join
       "C-,"   evil-leader/map
       ;"<escape>" 'tahti-esc
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
       "'"    'evil-goto-mark
       "`"    'evil-goto-mark-line
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
       "c SPC" 'tahti-toggle-comment
       "u" 'undo-tree-visualize
       "b" 'tahti/buffer
       "B" 'tahti/buffer-alternate
       "fo" 'helm-for-files       ;open file
       "fi" 'helm-insert-file
       "fw" 'helm-write-file      ;save as dialog
       "fh" 'tahti/helm-history-files
       "fc" 'tahti/config-files
       "ff" 'tahti/file
       "fF" 'tahti/file-alternate
       "FF" 'tahti/file-alternate
       "Ff" 'tahti/file-alternate
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
       "vg" 'magit-status
       "gn" 'next-buffer
       "gh" 'previous-buffer
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
)

(defun tahti-yas-keys ()
  (define-key yas-minor-mode-map "\C-c&" nil)
)

(defun tahti-cedet-keys ()
  (fill-keymap semantic-mode-map
    ;"C-c"   'previous-line
    ;"C-g"   mode-specific-map ;C-c -> C-g (old keyboard-quit)
  )
)

(defun tahti-helm-keys ()
    (fill-keymap helm-map
       "C-f"      'helm-execute-persistent-action
       "C-b"      'helm-find-files-down-one-level
       "C-S-n"    'helm-next-source
       "C-S-h"    'helm-previous-source
       "C-d"      'delete-backward-char
       "C-S-d"    'delete-forward-char
       "<left>"   'backward-char
       "<right>"  'forward-char
       "C-c C-g"  'helm-follow-mode
       "C-z"      nil
    )
  (define-key 'help-command "A" 'apropos)

)
(defun tahti-comint-keys()
  (fill-keymap comint-mode-map
               "C-M-t" 'comint-next-input
               "C-M-c" 'comint-previous-input
               "M-p" nil
               "M-n" nil)
)

(defun tahti-isearch-keys()
  ;(define-key isearch-mode-map [escape] 'isearch-cancel) ;help
)

(defun tahti-global-keys()
  (add-hook 'comint-mode-hook 'tahti-comint-keys)
  (add-hook 'helm-mode-hook   'tahti-helm-keys)
  (add-hook 'isearch-mode-hook 'tahti-isearch-keys)
  (add-hook 'evil-mode-hook 'tahti-evil-keys)

  (define-key key-translation-map "\C-f" "\C-g") ;keyboard quit
  (define-key key-translation-map "\C-g" "\C-c") ;go
  (define-key key-translation-map "\C-c" "\C-p") ;up
  (define-key key-translation-map "\C-p" "\C-t") ;transpose
  (define-key key-translation-map "\C-t" "\C-n") ;down
  (define-key key-translation-map "\C-n" "\C-f") ;left

  (define-key key-translation-map "\C-h" "\C-b") ;right
  (define-key key-translation-map "\C-b" "\C-h") ;help

  (fill-keymap 'global
    "C-x g" 'magit-status
    "C-+"   'text-scale-increase  ;;increase font
    "C--"   'text-scale-decrease  ;;decrease font
    "M-h"   'evil-window-left
    "M-n"   'evil-window-right
    "M-c"   'evil-window-up
    "M-t"   'evil-window-down
    ;"<f10>" 'tahti/helm-lacarte
    "C-<escape>" 'ESC-prefix
    "<escape>" 'tahti-esc
  )
  ;(fill-keymap minibuffer-local-map
    ;"C-c"   'previous-line
    ;"C-g"   mode-specific-map ;C-c -> C-g (old keyboard-quit)
  ;)
;;;; Rest of the keys ========================================
;; use shift + arrow keys to switch between visible buffers
;(require 'windmove)
  (windmove-default-keybindings)
)

(provide 'tahti-keys)
;;;; tahti-keys ends here