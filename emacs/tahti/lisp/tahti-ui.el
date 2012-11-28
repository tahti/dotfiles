;; Clean UI ========================================
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'tooltip-mode) (tooltip-mode -1))
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'blink-cursor-mode) (blink-cursor-mode -1))
;; menu bar==========================================
(push 'menu-bar+ el-get-packages)
(push 'rainbow-mode el-get-packages)
;;; fringes - vertical border margins==================
;; make the fringe (gutter) smaller the argument is a width in pixels for left and right(the default is 8)
(when (fboundp 'fringe-mode)
    (fringe-mode '(0 . 4)))
(setq visual-line-fringe-indicators '(nil right-curly-arrow))
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
(setq-default major-mode 'text-mode)
(setq-default cursor-type 'bar)
(setq-default cursor-in-non-selected-windows nil)

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


(setq font-lock-verbose nil)
;; enable functions ========================================
(put 'narrow-to-defun 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'set-goal-column 'disabled nil)
(fset 'yes-or-no-p 'y-or-n-p)

(provide 'tahti-ui)
;;; tahit-ui.el ends here
