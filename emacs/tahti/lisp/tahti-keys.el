;;; package --- Summary 
(require 'tahti-util)
(require 'tahti-func)
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
           (equal 'eclim-problems-mode major-mode)
           (equal 'info-mode major-mode)
           (equal 'compilation-mode major-mode) 
           (equal 'grep-mode major-mode) 
           (equal 'completion-list-mode major-mode) 
           (equal 'special-mode major-mode) 
           (equal 'apropos-mode major-mode)) 
       (kill-buffer-and-window))
      ((equal 'undo-tree-visualizer-mode major-mode) (undo-tree-visualizer-quit))
      (t (keyboard-escape-quit))))

(defun tahti-evil-keys ()
    (fill-keymap evil-visual-state-map
      "*"  'tahti-search-selection-forward
      "#"  'tahti-search-selection-backward
      "+"  'er/expand-region
      "-"  'er/contract-region
      "ac"  'er/mark-comment
      "aa"  'mark-whole-buffer
      "an"  'narrow-to-region
      "ah"  'widen
      "a|"  (lambda ()(interactive)
                     (setq current-prefix-arg '(4)) ;C-u
                     (call-interactively 'shell-command-on-region))
    )
    ;(fill-keymap evil-emacs-state-map
       ;"<escape>" 'evil-normal-state ;we do not want previous state but normal state
    ;)
    (fill-keymap evil-normal-state-map
       "c"    nil ;otherwise c is still 'evil-change
       "C"    nil
       "j"    'evil-change
       "J"    'evil-change-line
       "T"    'evil-join
       "p"    'evil-paste-before  ;p<->P when pasting
       "P"    'evil-paste-after
       "C-,"   evil-leader--default-map
       "+"    'evil-numbers/inc-at-pt
       "-"    'evil-numbers/dec-at-pt
       "'"    'tahti-goto-mark
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
       evil-leader/leader evil-leader--default-map   ;make leader work in motion mode
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
       "s" 'sunrise-cd
       "B" 'tahti/buffer-alternate
       "a=" 'underline-with-char-equal
       "a-" 'underline-with-char-minus
       "a~" 'underline-with-char-tilde
       "a^" 'underline-with-char-up
       "a+" 'underline-with-char-plus
       "at" 'adoc-toggle-title-type
       "fo" 'helm-for-files       ;open file
       "fi" 'helm-insert-file
       "fw" 'helm-write-file      ;save as dialog
       "fh" 'tahti/helm-history-files
       "fc" 'tahti/config-files
       "ff" 'tahti-file
       "fF" 'tahti-file-alternate
       "FF" 'tahti-file-alternate
       "Ff" 'tahti-file-alternate
       "fr" 'rename-file-and-buffer
       "yf" 'copy-file-name-to-clipboard
       "ys" 'copy-short-file-name-to-clipboard
       "." 'evil-ex
       "/" 'moccur
       "o" 'tahti-find-at-alias
       "O" 'tahti-find-helm-at-alias
       "w" 'save-buffer-always
       "W" 'save-some-buffers
       "k" 'kill-current-buffer
       ;"k" 'server-edit
       "K" 'kill-buffer-and-window
       "<" 'tahti-cd-alias
       "d" 'dired-jump
       "D" 'tahti-dired-alias
       ;"h" 'monky-status
       "n" 'split-window-horizontally
       ;"c" 'delete-window
       "gn" 'next-buffer
       "gh" 'previous-buffer
       "gq" 'fill-paragraph
       "," 'evil-repeat-find-char-reverse
       "?f" 'what-face
       "?c" 'what-cursor-position
       "?w" 'count-words
       "?a" 'display-prefix
       "tl" 'hl-line-mode
       "t SPC" 'tahti-whitespace-toggle
       "tr" 'rainbow-mode
       "h" 'display-local-help
       "+" 'tahti-highlight-word-or-selection
       "-" 'tahti-unhighlight-word-or-selection
       "m" 'compile)
   ;use the non-prefixed <leader> in magit’s and gnus’ modes:
   (setq evil-leader/no-prefix-mode-rx '("magit-.*-mode" "gnus-.*-mode" "comp.*-mode" "grep-mode" "special-mode" "info-mode"))

   (evil-ex-define-cmd "w[rite]" 'save-buffer-always)

   ;   "N" 'make-frame-command
   ;   "C" 'delete-frame


   ;   "s" 'tahti/switch-file
   ;   ";" 'tahti/end-prog-line

   ;fix enter behaviour
    (evil-define-key 'motion completion-list-mode-map (kbd "<return>") 'choose-completion)
    (evil-define-key 'motion completion-list-mode-map (kbd "RET") 'choose-completion)
    (evil-define-key 'motion browse-kill-ring-mode-map (kbd "<return>") 'browse-kill-ring-insert-and-quit)
    (evil-define-key 'motion browse-kill-ring-mode-map (kbd "RET") 'browse-kill-ring-insert-and-quit)
    (evil-define-key 'motion occur-mode-map (kbd "<return>") 'occur-mode-goto-occurrence)
    (evil-define-key 'motion occur-mode-map (kbd "RET") 'occur-mode-goto-occurrence);
)
;(defun tahti-jump ()
  ;(interactive)
;;(memq (followin-char(point)) '("(" ")" "[" "]" "}" "{"))
  ;(if (string-match "\\s\)\\|\\s\(" (string (following-char)))
    ;(call-interactively 'evil-jump-item)
    ;(call-interactively 'predictive-latex-jump-to-matching-delimiter)
    ;))

;(define-key map (kbd "j") 'next-line)
    ;(define-key map (kbd "k") 'previous-line)
    ;(define-key map (kbd "h") 'backward-char)
    ;(define-key map (kbd "l") 'forward-char)
    ;(define-key map (kbd "e") 'scroll-down)
    ;(define-key map (kbd "SPC") 'scroll-up)
    ;(define-key map (kbd "J") 'irfc-scroll-up-one-line)
    ;(define-key map (kbd "K") 'irfc-scroll-down-one-line)
    ;(define-key map (kbd ",") 'end-of-buffer)
    ;(define-key map (kbd ".") 'beginning-of-buffer)
    ;(define-key map (kbd "T") 'irfc-render-toggle)
    ;(define-key map (kbd "q") 'irfc-quit)
    ;(define-key map (kbd "o") 'irfc-follow)
    ;(define-key map (kbd "v") 'irfc-visit)
    ;(derine-key map (kbd "r") 'irfc-reference-goto)
    ;(define-key map (kbd "f") 'irfc-head-goto)
    ;(define-key map (kbd "F") 'irfc-head-number-goto)
    ;(define-key map (kbd "g") 'irfc-page-goto)
    ;(define-key map (kbd "N") 'irfc-page-next)
    ;(define-key map (kbd "P") 'irfc-page-prev)
    ;(define-key map (kbd ">") 'irfc-page-last)
    ;(define-key map (kbd "<") 'irfc-page-first)
    ;(define-key map (kbd "b") 'irfc-page-table)
    ;(define-key map (kbd "H") 'irfc-head-next)
    ;(define-key map (kbd "L") 'irfc-head-prev)
    ;(define-key map (kbd "G") 'irfc-table-jump)
    ;(define-key map (kbd "<tab>") 'irfc-rfc-link-next)
    ;(define-key map (kbd "<backtab>") 'irfc-rfc-link-prev)

(defun tahti-irfc-keys()
  (fill-keymap irfc-mode-map
         "j" nil 
         "k" nil
         "h" nil
         "l" nil
         "e" nil
         "SPC" nil
         "J" nil
         "K" nil
         "," nil
         "." nil
         "R" 'irfc-render-toggle
         "T" 'irfc-head-next
         "C" 'irfc-head-prev
         "p" 'irfc-page-goto
         "q" nil
         "o" 'irfc-follow
         "v" nil
         "r" 'irfc-reference-goto
         "f" nil
         "F" nil
         "g" nil
         "P" nil
         ">" nil
         "<" nil
         "b" 'irfc-page-table
         "L" nil
         "G" nil
         "C-b" 'irfc-page-prev
         "C-f" 'irfc-page-next
         "N" nil
         "H" nil
   )

   ;(evil-leader/set-key
       ;"SPC" 'irfc-table-jump
       ;"e" 'read-only-mode
   ;)

)

(defun tahti-latex-keys()
   (evil-leader/set-key
       "ie" 'LaTeX-environment
       "im" 'TeX-insert-macro
       "is" 'LaTeX-section
       "&"  'reftex-view-crossref
       "ic" 'reftex-citation
       "("  'reftex-label
       "il" 'reftex-label
       ")"  'reftex-reference
       "ir" 'reftex-reference
       "ii" 'LaTeX-insert-item  ;insert \item
       "ifb" '(lambda () (interactive)(TeX-font nil ?\C-b)) ;insert bold
       "ifc" '(lambda () (interactive)(TeX-font nil ?\C-c)) ;
       "ife" '(lambda () (interactive)(TeX-font nil ?\C-e))
       "ifi" '(lambda () (interactive)(TeX-font nil ?\C-i))
       "ifr" '(lambda () (interactive)(TeX-font nil ?\C-r))
       "ifs" '(lambda () (interactive)(TeX-font nil ?\C-s))
       "ift" '(lambda () (interactive)(TeX-font nil ?\C-t)) 
       "ifd" '(lambda () (interactive)(TeX-font nil ?\C-d)) ;delete font
       "fd"  '(lambda () (interactive)(TeX-font nil ?\C-d)) ;delete font
       "cr" 'TeX-comment-or-uncomment-paragraph
       "tm" 'LaTeX-math-mode
       "gq" '(lambda () (interactive)(fill-paragraph t t))
       "qp" 'LaTeX-fill-paragraph
       "qe" 'LaTeX-fill-environment
       "qs" 'LaTeX-fill-section
       "qr" '(lambda () (interactive)(progn (call-interactively 'LaTeX-fill-region) (call-interactively 'evil-normal-state)))
       "zz" 'TeX-fold-dwim
       "zb" 'TeX-fold-buffer
       "zB" 'TeX-fold-clearout-buffer
       "zr" 'TeX-fold-region
       "zR" 'TeX-fold-clearout-region
       "zp" 'TeX-fold-paragraph
       "zP" 'TeX-fold-clearout-paragraph
       "zm" 'TeX-fold-macro
       "zM" 'TeX-fold-clearout-item
       "ze" 'TeX-fold-env
       "zE" 'TeX-fold-clearout-item
       "zx" 'TeX-fold-math
       "zX" 'TeX-fold-clearout-item
       "zc" 'TeX-fold-comment
       "zu" 'TeX-fold-clearout-item
       "pd" 'preview-document
       "pD" 'preview-clearout-document
       "pb" 'preview-buffer
       "pB" 'preview-clearout-buffer
       "pe" 'preview-environment
       "ps" 'preview-section
       "pS" 'preview-clearout-section
       "pr" 'preview-region
       "pR" 'preview-clearout
       "pp" 'preview-at-point
       "pu" 'preview-clearout-at-point
       "pf" 'preview-cache-preamble
       "pF" 'preview-cache-preamble-off
       "v"  'TeX-view
   )
   ;undefine some of cdlatex keys
  (fill-keymap cdlatex-mode-map
       "$" nil
       "(" nil
       "{" nil
       "[" nil
       "|" nil
       "<" nil
   )

   ;(evil-define-key 'normal predictive-latex-map "%" 'tahti-jump)
   ;(evil-define-key 'visual predictive-latex-map "%" 'tahti-jump)
  (define-key isearch-mode-map [escape] 'isearch-cancel) ;help
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
       ;"C-f"      'helm-execute-persistent-action
       ;"C-b"      'helm-find-files-down-one-level
       "C-S-n"    'helm-next-source
       "C-S-h"    'helm-previous-source
       "C-f"      'helm-next-page
       "C-b  "    'helm-previous-page
       "C-d"      'delete-backward-char
       "C-S-d"    'delete-forward-char
       "<left>"   'backward-char
       "<right>"  'forward-char
       "C-c C-g"  'helm-follow-mode
       "C-z"      nil
    )
  (define-key 'help-command "A" 'apropos)

)
;keys for sunrise commander
(defun tahti-sr-keys ()
  (define-key sr-mode-map "\M-n"        'sr-change-window)
  (define-key sr-mode-map "\M-h"        'sr-change-window)
  (define-key sr-mode-map "\M-t"        'evil-window-down) ;was 'sr-traspose-panes
  (define-key sr-mode-map "t"           'dired-next-line)
  (define-key sr-mode-map "c"           'dired-previous-line)
  (define-key sr-mode-map "h"           'sr-dired-prev-subdir)
  (define-key sr-mode-map "n"           'sr-advertised-find-file)
  (define-key sr-mode-map [M-tab]       'sr-synchronize-panes)
  (define-key sr-mode-map "/"           'sr-fuzzy-narrow)
  (define-key sr-mode-map (kbd "C-/")   'sr-find-grep)
  (define-key sr-mode-map (kbd "S-<f4>") 'sr-create-files)
  ;disable mouse
  (define-key sr-mode-map [mouse-1] nil)
  (define-key sr-mode-map [mouse-movement] nil)
  ; "g" removes fuzzy filter - refreshes
  (define-key sr-mode-map ","           evil-leader--default-map) 
  (define-key sr-mode-map ":"           'evil-ex) 
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

(defun tahti-arc-keys()
  (define-key archive-mode-map ","       evil-leader--default-map) 
)

(defun tahti-eclim-problems-keys()
  ;(define-key eclim-problems-mode-map  "a" 'eclim-problems-show-all)
  ;(define-key eclim-problems-mode-map "e" 'eclim-problems-show-errors)
  (define-key eclim-problems-mode-map "r" 'eclim-problems-buffer-refresh)
  ;(define-key eclim-problems-mode-map "q" 'eclim-quit-window)
  ;(define-key eclim-problems-mode-map "w" 'eclim-problems-show-warnings)
  (define-key eclim-problems-mode-map "l" 'eclim-problems-toggle-filefilter)
  (define-key eclim-problems-mode-map "f" 'eclim-problems-correct)
  (define-key eclim-problems-mode-map "n" 'eclim-problems-open-current)
  (define-key eclim-problems-mode-map "t" 'next-line)
  (define-key eclim-problems-mode-map "c" 'previous-line)
  (define-key eclim-problems-mode-map ","  evil-leader--default-map)
  (define-key eclim-problems-mode-map "/" 'evil-search-forward)
  (define-key eclim-problems-mode-map "K" 'evil-search-previous)
  (define-key eclim-problems-mode-map "k" 'evil-search-next)
  (define-key eclim-problems-mode-map "G" 'end-of-buffer)
  ;(define-key eclim-problems-mode-map "gg" 'beggining-of-buffer)
  (define-key eclim-problems-mode-map "\C-n" 'scroll-up-command)
  (define-key eclim-problems-mode-map "\C-h" 'scroll-down-command)

)
(defun tahti-completion-list-mode-keys()
 "Add keybindings to `completion-list-mode-map'.\n
  Binds `next-line to \"t\".\n
  Binds `previos-line to \"c\".\n"
  (define-key completion-list-mode-map "t"          'next-line)
  (define-key completion-list-mode-map "c"          'previous-line)
  (define-key completion-list-mode-map "n"          'choose-completion)
)

(defvar eclim-java-show-documentation-map
  (let ((map (make-keymap)))
    (suppress-keymap map)
    (define-key map (kbd "<tab>") 'forward-button)
    (define-key map (kbd "S-<tab>") 'backward-button)
    (define-key map (kbd "q") 'eclim-quit-window)
    (define-key map (kbd "<escape>") 'eclim-quit-window)
    (define-key map (kbd "t") 'next-line)
    (define-key map (kbd "c") 'previous-line)
    (define-key map (kbd "n") 'forward-button)
    (define-key map (kbd "h") 'backward-button)
    map))

(defun tahti-eclim-mode-keys()
   (evil-leader/set-key
       "m"  'eclim-maven-lifecycle-phase-run
       "pg" 'eclim-project-goto
       "pb" 'eclim-project-build
       "pm" 'eclim-manage-projects
       "ee" 'tahti-problems-open
       "ep" 'eclim-problems
       "ec" 'eclim-problems-correct
       "ed" 'eclim-java-show-documentation-for-current-element
       "eh" 'eclim-java-hierarchy
       "es" 'start-eclimd
       "ek" 'stop-eclimd
       "gd" 'eclim-java-find-declaration
       "gr" 'eclim-java-find-references
       "gt" 'eclim-java-find-type
       "gg" 'eclim-java-find-generic
       "cr" 'eclim-java-refactor-rename-symbol-at-point
       "ci" 'eclim-java-import-organize
       "cf" 'eclim-java-implement
       "cd" 'eclim-java-doc-comment 
       ;"cq" 'eclim-java-format
       "cq" (lambda () (interactive) (eclim-java-format) 
              (save-excursion
                (goto-char (point-min))
                (when (search-forward "\t" nil t)
                  (untabify (1- (point)) (point-max)))
                nil))
       "cg" 'eclim-java-generate-getter-and-setter
       ;"cc" 'eclim-java-constructor
       "gh" 'tahti-javadoc
       )
)

(defun tahti-nxml-mode-keys()
  (define-key nxml-mode-map "\M-h"          'evil-window-left)
)

(defun tahti-global-keys()
  (add-hook 'comint-mode-hook 'tahti-comint-keys)
  (add-hook 'helm-mode-hook   'tahti-helm-keys)
  (add-hook 'isearch-mode-hook 'tahti-isearch-keys)
  (add-hook 'evil-mode-hook 'tahti-evil-keys)
  (add-hook 'sr-mode-hook 'tahti-sr-keys)
  (add-hook 'arcchive-mode-hook 'tahti-arc-keys)
  (add-hook 'eclim-problems-mode-hook 'evil-emacs-state)
  (add-hook 'eclim-problems-mode-hook 'tahti-eclim-problems-keys)
  (add-hook 'eclim-mode-hook 'tahti-eclim-mode-keys)
  (add-hook 'completion-list-mode-hook 'tahti-completion-list-mode-keys)
  (add-hook 'nxml-mode-hook 'tahti-nxml-mode-keys)

  (define-key key-translation-map "\C-f" "\C-g") ;keyboard quit
  (define-key key-translation-map "\C-g" "\C-c") ;go
  (define-key key-translation-map "\C-c" "\C-p") ;up
  (define-key key-translation-map "\C-p" "\C-t") ;transpose
  (define-key key-translation-map "\C-t" "\C-n") ;down
  (define-key key-translation-map "\C-n" "\C-f") ;left

  (define-key key-translation-map "\C-h" "\C-b") ;right
  (define-key key-translation-map "\C-b" "\C-h") ;help

  (fill-keymap 'global
    "C-h y" 'yas/describe-tables  ;;help for yasnippets
    "C-x g" 'magit-status
    "C-+"   'text-scale-increase  ;;increase font
    "C--"   'text-scale-decrease  ;;decrease font
    "M-h"   'evil-window-left
    "M-n"   'evil-window-right
    "M-c"   'evil-window-up
    "M-t"   'evil-window-down
    "M-/"   'hippie-expand
    "M-+"   'text-scale-adjust
    "M--"   'text-scale-adjust
    ;"<f10>" 'tahti/helm-lacarte
    "<f7>" 'flyspell-mode
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
;;; tahti-keys ends here
