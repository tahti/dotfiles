(custom-set-variables '(evil-overriding-maps
  '((Buffer-menu-mode-map . nil)
    (color-theme-mode-map . nil)
    (comint-mode-map . nil)
    (compilation-mode-map . nil)
    (grep-mode-map . nil)
    (dictionary-mode-map . nil)
    (ert-results-mode-map . motion)
    (irfc-mode-map . nil)
    (Info-mode-map . motion)
    (speedbar-key-map . nil)
    (speedbar-file-key-map . nil)
    (speedbar-buffers-key-map . nil))))

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

;(let ((search-functions
       ;'(forward
         ;backward
         ;word-forward
         ;word-backward
         ;unbounded-word-forward
         ;unbounded-word-backward
         ;next
         ;previous)))
  ;(dolist (fun search-functions)
    ;(let ((isearch (intern (format "evil-search-%s" fun)))
          ;(evil-search (intern (format "evil-ex-search-%s" fun))))
        ;(substitute-key-definition
         ;isearch evil-search evil-motion-state-map))))

(push 'evil-leader el-get-packages)
(push 'evil-numbers el-get-packages)
(push 'evil-surround el-get-packages)
(push 'evil-matchit el-get-packages)
(push 'evil el-get-packages)
(push 'undo-tree el-get-packages)
(push 'highlight el-get-packages)
;(push 'evil-search-highlight-persist el-get-packages)
;(push 'evil-search-highlight-persist el-get-packages)


;(defun tahti-search-selection-forward (beg end)
  ;"search for selected text in forward direction"
  ;(interactive "r")
  ;(when (evil-visual-state-p)
    ;(let (
          ;(selection (buffer-substring-no-properties beg end))
         ;)
      ;(evil-exit-visual-state)
      ;(setq isearch-forward t)
      ;(evil-search (regexp-quote selection) t t)
    ;)
  ;)
;)


(defun tahti-goto-mark ()
  (interactive)
  (call-interactively 'evil-goto-mark)
  (call-interactively 'evil-scroll-line-to-center)
)
 ;(require 'smex)
(defun tahti-after-evil ()
  (defun tahti/begin-search (beg end direction do_mark)
    (when (evil-visual-state-p)
      (evil-exit-visual-state)
      (let ((found)
            (selection (regexp-quote (buffer-substring-no-properties beg end))))
        (if (eq evil-search-module 'isearch)
            (progn
              (setq isearch-forward direction)
              (setq found (evil-search selection direction t)))
          (let ((pattern (evil-ex-make-search-pattern selection))
                (direction (if direction 'forward 'backward)))
            (setq evil-ex-search-direction direction)
            (setq evil-ex-search-pattern pattern)
            (evil-ex-search-activate-highlight pattern)
            ;; update search history unless this pattern equals the
            ;; previous pattern
            (unless (equal (car-safe evil-ex-search-history) selection)
              (push selection evil-ex-search-history))
            (evil-push-search-history selection (eq direction 'forward))
            (setq found (evil-ex-search-next))))
        (when (and do_mark found)
          (push-mark (+ (point) (- end beg)) nil t))
        )))
  (defun tahti-search-clear-highlight ()
       "Clear evil-search or evil-ex-search persistent highlights."
       (interactive)
       (case evil-search-module
         ('isearch (evil-search-highlight-persist-remove-all))
         ('evil-search (evil-ex-nohighlight))))
  (evil-define-motion tahti-search-selection-backward (beg end)
    "Search for selected text in backward direction."
    :jump t
    :repeat nil
    (interactive "<r>")
    (tahti/begin-search beg end nil nil)
  )

  (evil-define-motion tahti-search-selection-forward (beg end)
    "Search for selected text in forward direction."
    :jump t
    :repeat nil
    (interactive "<r>")
    (tahti/begin-search beg end t nil)
  )

   (evil-define-motion tahti-search-and-select-backward (beg end)
    "Search for selected text in backward direction."
    :jump t
    :repeat nil
    (interactive "<r>")
    (tahti/begin-search beg end nil t)
  )

   (evil-define-motion tahti-search-and-select-forward (beg end)
    "Search for selected text in forward direction."
    :jump t
    :repeat nil
    (interactive "<r>")
    (tahti/begin-search beg end t t)
  )

  (evil-add-command-properties 'evil-goto-mark :jump t)
  (evil-remove-command-properties 'evil-forward-section-begin :jump)
  (evil-remove-command-properties 'evil-find-char :jump)
  (evil-remove-command-properties 'evil-find-char-to :jump)
  (evil-remove-command-properties 'evil-find-char-backward :jump)
  (evil-remove-command-properties 'evil-find-char-to-backward :jump)
  (evil-remove-command-properties 'evil-repeat-find-char :jump)
  (evil-remove-command-properties 'evil-repeat-find-reverse :jump)
  (evil-remove-command-properties 'evil-forward-sentence :jump)
  (evil-remove-command-properties 'evil-backward-sentence :jump)
  (require 'evil-matchit)
  (global-evil-matchit-mode 1)
  ;(require 'undo-tree)
  (global-undo-tree-mode 1)
  (message "Evil loading")
  ;Indicator settings
     (setq evil-normal-state-tag (propertize "N" 'face '((:background "white"   :foreground "black")))
        evil-insert-state-tag    (propertize "I" 'face '((:background "yellow"  :foreground "black")))
        evil-visual-state-tag    (propertize "V" 'face '((:background "purple"  :foreground "black")))
        evil-replace-state-tag   (propertize "R" 'face '((:background "red")))
        evil-operator-state-tag  (propertize "O" 'face '((:background "#FFDD00":foreground "black")))
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
  (set-face-background 'lazy-highlight "#5F8787") ; make searching highlight same as in vim

  (evil-mode 1)
   ;(custom-set-variables
   ;'(evil-search-module (quote evil-search)))
  (evil-select-search-module 'evil-search-module 'evil-search)
  ;(evil-flash-hook nil)
  ;(setq evil-flash-delay 0)
  ;(setq lazy-highlight-cleanup nil)
  ;(setq lazy-highlight-initial-delay 0)
  ;(require 'advice)
  ;(require 'evil-search)
  ;(require 'highlight)

)
(provide 'tahti-evil)
;;;; tahti-evil.el ends here
