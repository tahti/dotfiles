(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(custom-set-variables
 '(tab-width 2)
 '(indent-tabs-mode nil)
 '(show-paren-mode t)
 '(fill-column 80)
 '(set-mark-command-repeat-pop t)
)

;;;; vimvars =========================================
(push 'vimvars el-get-packages)
(defun tahti-after-vimvars ()
  (add-hook 'find-file-hook 'vimvars-obey-vim-modeline))
  (tahti-global-keys) ;run at the last package

(provide 'tahti-editor)

