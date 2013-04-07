(require 'tahti-keys)
;;do not lock the files using symbolic links - works for emacs 24.3 from trunk
(setq create-lockfiles nil)
;;comment empty lines
(setq comment-empty-lines 'eol)
;;reuse existing buffers
(setq ns-pop-up-frames nil)
 ;; use utf-8 environment as default dammit
(setq locale-coding-system 'utf-8)
(set-language-environment 'UTF-8)
(set-locale-environment "UTF-8")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
;;;extended help mode ===============================
(push 'info+ el-get-packages)
(push 'help-mode+ el-get-packages)
(push 'help-fns+ el-get-packages)
(push 'help+ el-get-packages)
(push 'color-moccur el-get-packages)
(defun tahti-after-help+ ()
  (require 'info+)
  (require 'help-mode+)
  (require 'help-fns+)
  (require 'help+)
  (require 'color-moccur)
)
(custom-set-variables
 '(tab-width 2)
 '(indent-tabs-mode nil)
 '(show-paren-mode t)
 '(fill-column 80)
 '(set-mark-command-repeat-pop t)
)
;;; whitespace-mode

(custom-set-variables
;(face tabs spaces trailing lines space-before-tab newline indentation empty space-after-tab space-mark tab-mark newline-mark)
  '(whitespace-style'(face trailing indentation:space tab-mark newline-mark) )
  '(whitespace-display-mappings
          '((space-mark   ?\    [?\xB7] )	; space
            (space-mark   ?\xA0 )	; hard space
            (newline-mark ?\n  [?\xA4 ?\n])	; end-of-line
            (tab-mark   ?\t   [?\xBB ?\t])
           )
  )
)
(push 'column-marker el-get-packages)
(defun tahti-after-column-marker()
  (custom-set-variables
    '(column-marker-1-face '((t (:background "#AA501A"))))
    '(column-marker-2-face '((t (:background "#960050"))))
  )
)
(defun tahti-whitespace-toggle()
  (interactive)
  (call-interactively 'whitespace-mode)
  (if whitespace-mode 
    (progn (column-marker-1 80)
           (column-marker-2 120))
    (progn (column-marker-1 (16))
           (column-marker-2 (16)))
  )
)
;;; w3m ==============================================
(setq browse-url-browser-function 'w3m-browse-url)
(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
 ;; optional keyboard short-cut
;;;expand selection region ==========================
(push 'expand-region el-get-packages)

;;; woman - help files ===============================
(setq-default woman-use-own-frame nil
              woman-use-topic-at-point t
              woman-imenu t)
;
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

;;;; vimvars =========================================
(push 'vimvars el-get-packages)
(defun tahti-after-vimvars ()
  (add-hook 'find-file-hook 'vimvars-obey-vim-modeline))
  (tahti-global-keys) ;run at the last package

(provide 'tahti-editor)

;;; pretty-mode ======================================
(setq pp^L-^L-string (concat (make-string 30 ? ) "⁂" (make-string 30 ? ))
      pp^L-^L-string-pre "")

(push 'pretty-mode el-get-packages)
  (when (fboundp 'pretty-control-l-mode)
    (pretty-control-l-mode 1))
  (defun tahti-after-pretty-mode ()
    (dolist (mode '(python-mode c-mode java-mode cpp-mode))
            (pretty-add-keywords mode '(("="  . "←")
                                        ("==" . "≡"))))
  (global-pretty-mode 1)
)
;;; ==================================================
;;; scratch ====================
(defun save-a-scratch ()
  "Prevent *scratch* buffer from being killed.
Intended as `kill-buffer-query-functions' fun."
  (not (string= "*scratch*" (buffer-name))))

(push #'save-a-scratch kill-buffer-query-functions)

;;; ==============================
(setq multi-term-dedicated-select-after-open-p t)
;; enable subword-mode that lets you move by camelCase
(global-subword-mode 1)

(provide 'tahti-editor)
;;; tahit-ui.el ends here

