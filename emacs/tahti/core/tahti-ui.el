;; Clean UI ========================================
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'tooltip-mode) (tooltip-mode -1))
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'blink-cursor-mode) (blink-cursor-mode -1))
;; ==================================================
;;; 
;;; backups & autosave===============================
(add-hook 'find-file-hook 'auto-save-mode)
(setq auto-save-timeout 20)
;; make a shell script executable automatically on save
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)
(setq auto-save-file-name-transforms
          `((".*" ,autosave-directory t)))

;;; recent files ===================================
(setq recentf-auto-cleanup 'never)
(setq recentf-max-saved-items 200)
(require-and-exec 'recentf
   (recentf-mode 1)
   (setq recentf-exclude '(
                           "\\.recentf"
                           "\\.ido\\.last"
                           "\\.keychain/.*?-sh\\(-gpg\\)?"
                           ))
   (add-hook 'kill-emacs-hook #'recentf-cleanup)
   )
;;; timestamps =======================================
;; when there's "Time-stamp: <>" in the first 10 lines of the file
(setq time-stamp-active t
      ;; check first 10 buffer lines for Time-stamp: <>
      time-stamp-line-limit 10
      time-stamp-format "%04y-%02m-%02d %02H:%02M:%02S (%u)") ; date format
(add-hook 'write-file-hooks 'time-stamp) ; update when saving
;;; tramp ============================================
(setq tramp-default-method "ssh")
;;; saveplace ========================================
(setq-default save-place t)
(require 'saveplace)
;;;; vimvars =========================================
(require 'vimvars)
(add-hook 'find-file-hook 'vimvars-obey-vim-modeline)
;;; savehist  ========================================
(setq savehist-additional-variables
      ;; search entries
      '(search ring regexp-search-ring)
      ;; save every three minutes
      savehist-autosave-interval 180)
(savehist-mode t)
;;ack-and-a-half ====================================
(defalias 'ack 'ack-and-a-half)
(defalias 'ack-same 'ack-and-a-half-same)
(defalias 'ack-find-file 'ack-and-a-half-find-file)
(defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)
;;; ido-mode =========================================
(ido-mode 'files)
(ido-everywhere 1)
(add-to-list 'ido-ignore-directories "target")
(add-to-list 'ido-ignore-directories "node_modules")
;;; w3m ==============================================
;;;
(setq browse-url-browser-function 'w3m-browse-url)
(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
 ;; optional keyboard short-cut
;;; pretty-mode ======================================
(setq pp^L-^L-string (concat (make-string 30 ? ) "⁂" (make-string 30 ? ))
      pp^L-^L-string-pre "")
(when (fboundp 'pretty-control-l-mode)
  (pretty-control-l-mode 1))
(require-and-exec 'pretty-mode
  (dolist (mode '(python-mode c-mode java-mode cpp-mode))
          (pretty-add-keywords mode '(("="  . "←")
                                      ("==" . "≡"))))
  (global-pretty-mode 1))
;;; woman - help files ===============================
(setq-default woman-use-own-frame nil
              woman-use-topic-at-point t
              woman-imenu t)
;;; keychain-mode ====================================
;(require-and-exec 'keychain-environment
  ;(run-with-timer 100 (* 5 60) 'keychain/refresh))
;;uniquify - naming same buffers =========================
(require-and-exec 'uniquify
                  (setq uniquify-buffer-name-style 'post-forward
                        uniquify-separator "/"
                        uniquify-after-kill-buffer-p t       ; rename after killing uniquified 
                        uniquify-ignore-buffers-re "^\\*"))  ; don't muck with special buffers 
;;; eldoc ============================================
(require-and-exec 'eldoc
  (add-to-hooks 'turn-on-eldoc-mode 
                    '(python-mode-hook
                      emacs-lisp-mode-hook
                      lisp-interaction-mode-hook
                      ielm-mode-hook
                      ))
  (eldoc-add-command 'autopair-insert-opening))
;;; fringe ===========================================
;; make the fringe (gutter) smaller
;; the argument is a width in pixels (the default is 8)
(if (fboundp 'fringe-mode)
    (fringe-mode 4))
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
;;; rebuilder ========================================
;;; Use string syntax for re-builder
(require 're-builder)
(setq reb-re-syntax 'string)
;;; ==========================================================
;; automatically indenting yanked text if in programming-modes
(defvar yank-indent-modes '(python-mode LaTeX-mode TeX-mode)
  "Modes in which to indent regions that are yanked (or yank-popped). Only
modes that don't derive from `prog-mode' should be listed here.")

(defvar yank-advised-indent-threshold 1000
  "Threshold (# chars) over which indentation does not automatically occur.")

(defun yank-advised-indent-function (beg end)
  "Do indentation, as long as the region isn't too large."
  (if (<= (- end beg) yank-advised-indent-threshold)
      (indent-region beg end nil)))

(defadvice yank (after yank-indent activate)
  "If current mode is one of 'yank-indent-modes,
indent yanked text (with prefix arg don't indent)."
  (if (and (not (ad-get-arg 0))
           (or (derived-mode-p 'prog-mode)
               (member major-mode yank-indent-modes)))
      (let ((transient-mark-mode nil))
    (yank-advised-indent-function (region-beginning) (region-end)))))

(defadvice yank-pop (after yank-pop-indent activate)
  "If current mode is one of 'yank-indent-modes,
indent yanked text (with prefix arg don't indent)."
  (if (and (not (ad-get-arg 0))
           (or (derived-mode-p 'prog-mode)
               (member major-mode yank-indent-modes)))
    (let ((transient-mark-mode nil))
    (yank-advised-indent-function (region-beginning) (region-end)))))

;; other packages ========================================
(setq hl-paren-colors '("#9C0F0F" "#BF0303" "#E20800" "#E85752" "#F08682" "#F9CCCA"))
(require-and-exec 'highlight-parentheses
                  (add-hook 'find-file-hook 'highlight-parentheses-mode))
;;; ==========================================================
;; Settings ==================================================
;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)
;autoinsert paired bracket
(electric-pair-mode t)
;; enable subword-mode that lets you move by camelCase
(global-subword-mode 1)
;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)
(setq doc-view-continuous t)
(setq frame-title-format "emacs %b - <%f>"
      icon-title-format "emacs %b")
;; no, my sentences don't end with two instead of one space
(setq sentence-end-double-space nil)
(mouse-avoidance-mode 'cat-and-mouse)
(setq comment-style 'align)
(setq x-select-enable-primary t)
(setq use-dialog-box nil)
;; disable alarm bell and visual bell
(setq visible-bell nil)
(setq ring-bell-function 'ignore)
;; disable startup screen
(setq inhibit-startup-screen t)
;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

(setq ediff-split-window-function 'split-window-horizontally
      ediff-window-setup-function 'ediff-setup-windows-plain)


;; point not keep screen position while scrolling
(setq scroll-preserve-screen-position nil)
(scroll-bar-mode -1)

;; use utf-8 environment as default
(set-language-environment 'UTF-8)
(set-locale-environment "UTF-8")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)


;; Always rescan buffer for imenu
(set-default 'imenu-auto-rescan t)

;; indentation and tab stops
(setq-default tab-stop-list (range 2 160 2)
              indent-tabs-mode nil     ; no nasty tabs i say!
              tab-width 2)
(setq-default fill-column 80)
(auto-fill-mode 't)
(setq tab-always-indent 'complete)

(setq dabbrev-case-replace nil)


;;; ========================================
;; mixedCase to small_words_with_underscores (visually)
(setq glasses-separate-parentheses-p nil
      glasses-uncapitalize-p t
      glasses-uncapitalize-regexp "[a-zA-Z]")

(setq-default major-mode 'text-mode)
(setq-default cursor-type 'bar)

(setq font-lock-verbose nil)
;; enable functions ========================================
(put 'narrow-to-defun 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'set-goal-column 'disabled nil)
(fset 'yes-or-no-p 'y-or-n-p)
;; ==================================================
;;; scratch ====================
(defun save-a-scratch ()
  "Prevent *scratch* buffer from being killed.
Intended as `kill-buffer-query-functions' fun."
  (not (string= "*scratch*" (buffer-name))))

(push #'save-a-scratch kill-buffer-query-functions)
;;; ==============================

(setq multi-term-dedicated-select-after-open-p t)

(provide 'tahti-ui)
;;; tahit-ui.el ends here
