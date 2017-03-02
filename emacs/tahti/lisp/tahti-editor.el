(require 'tahti-keys)
;;do not lock the files using symbolic links - works for emacs 24.3 from trunk
(setq create-lockfiles nil)
;;comment empty lines
(setq comment-empty-lines 'eol)
;;reuse existing buffers
(setq ns-pop-up-frames nil)
;;
;;
;(defadvice find-file (around find-file-line-number
                             ;(filename &optional wildcards)
                             ;activate)
  ;"Recenters the screen after going to a line in the file.";;
  ;(recenter-top-bottom)
;)
 ;; use utf-8 environment as default dammit
(setq locale-coding-system 'utf-8)
(set-language-environment 'utf-8)
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
(push 'goto-last-change  el-get-packages)

;(push 'highlight-symbol el-get-packages)

;;;flycheck instead of flymake=======================
(push 'flycheck el-get-packages)
  ;(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

(defun tahti-after-help+ ()
  (require 'info+)
  (require 'help-mode+)
  (require 'help-fns+)
  (require 'help+)
  (require 'color-moccur)
)

;(defun tahti-after-highlight-symbol()
  ;(require 'highlight-symbol)
;)

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
;prevent emacs from asking about closing the file
(defun server-remove-kill-buffer-hook () (remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)) (add-hook 'server-visit-hook 'server-remove-kill-buffer-hook) 

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
(setq browse-url-browser-function 'browse-url-firefox
            browse-url-new-window-flag t
            browse-url-firefox-new-window-is-tab t)
;(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
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

(tahti-global-keys) ;run at the last package


;;; pretty-mode ======================================
(setq pp^L-^L-string (concat (make-string 30 ? ) "⁂" (make-string 30 ? ))
      pp^L-^L-string-pre "")

(push 'pretty-mode el-get-packages)
  (when (fboundp 'pretty-control-l-mode)
    (pretty-control-l-mode 1))
  (defun tahti-after-pretty-mode ()
    ;(dolist (mode '(python-mode c-mode java-mode cpp-mode))
            ;(pretty-add-keywords mode '(("="  . "←")
                                        ;("==" . "≡"))))
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

;;;=============highlighting==============================
;;;
(global-hi-lock-mode 1)

(defun tahti-highlight-word-case-insensitive (word color)
  (setq word (replace-regexp-in-string "a" "\[aA\]" word) )
  (setq word (replace-regexp-in-string "b" "\[bB\]" word) )
  (setq word (replace-regexp-in-string "c" "\[cC\]" word) )
  (setq word (replace-regexp-in-string "d" "\[dD\]" word) )
  (setq word (replace-regexp-in-string "e" "\[eE\]" word) )
  (setq word (replace-regexp-in-string "f" "\[fF\]" word) )
  (setq word (replace-regexp-in-string "g" "\[gG\]" word) )
  (setq word (replace-regexp-in-string "h" "\[hH\]" word) )
  (setq word (replace-regexp-in-string "i" "\[iI\]" word) )
  (setq word (replace-regexp-in-string "j" "\[jJ\]" word) )
  (setq word (replace-regexp-in-string "k" "\[kK\]" word) )
  (setq word (replace-regexp-in-string "l" "\[lL\]" word) )
  (setq word (replace-regexp-in-string "m" "\[mM\]" word) )
  (setq word (replace-regexp-in-string "n" "\[nN\]" word) )
  (setq word (replace-regexp-in-string "o" "\[oO\]" word) )
  (setq word (replace-regexp-in-string "p" "\[pP\]" word) )
  (setq word (replace-regexp-in-string "r" "\[rR\]" word) )
  (setq word (replace-regexp-in-string "s" "\[sS\]" word) )
  (setq word (replace-regexp-in-string "t" "\[tT\]" word) )
  (setq word (replace-regexp-in-string "u" "\[uU\]" word) )
  (setq word (replace-regexp-in-string "w" "\[wW\]" word) )
  (setq word (replace-regexp-in-string "v" "\[vV\]" word) )
  (setq word (replace-regexp-in-string "x" "\[xX\]" word) )
  (setq word (replace-regexp-in-string "y" "\[yY\]" word) )
  (setq word (replace-regexp-in-string "z" "\[zZ\]" word) )
  (setq word (replace-regexp-in-string "\\\\" "\\\\\\\\" word))
  (setq word (replace-regexp-in-string "\\*" "\\\\*" word))
  (setq word (replace-regexp-in-string "\\+" "\\\\+" word))
  (setq word (replace-regexp-in-string "\\[" "\\\\[" word))
  (setq word (replace-regexp-in-string "\\]" "\\\\]" word))
  ;; (insert word)
  (let ((case-fold-search t))
    (highlight-regexp word color) )  )


(defun tahti-highlight-word-or-selection ()
  (interactive)
  (let (word tahti-hi-colors pp )
    (setq pp (point))
    (if (region-active-p)    ( setq word (buffer-substring  (region-beginning)  (region-end) ))
      (let (beg end)
	(setq beg (progn (forward-word) (point)))
	(setq end (progn  (backward-word)  (point)))
	(setq word (buffer-substring  beg end ))
	))
    (message "%s" word)
    (setq tahti-hi-colors '(
    			 "hi-green"
    			 "hi-yellow"
    			 "hi-blue"
    			 "hi-pink"
                 ;;"hi-red-b"
    			 ;; "hi-black-b"
    			 ;; "hi-black-hb"
    			 ;; "hi-blue-b"
    		 	 ;; "hi-green-b"
    			 ))
    (tahti-highlight-word-case-insensitive word  (nth (% (length hi-lock-interactive-patterns) (safe-length tahti-hi-colors)) tahti-hi-colors))
    (goto-char pp)
    (push word search-ring))
  (evil-exit-visual-state))

(defun tahti-unhighlight-word-or-selection ()
  (interactive)
  (if (> (length hi-lock-interactive-patterns) 0) (unhighlight-regexp (car (car hi-lock-interactive-patterns))) (message "No highlighting to remove"))
  )
;; do not ask question about running processes when closing emacs TODO replace flet?
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (flet ((process-list ())) ad-do-it))

(provide 'tahti-editor)
;;; tahit-ui.el ends here

