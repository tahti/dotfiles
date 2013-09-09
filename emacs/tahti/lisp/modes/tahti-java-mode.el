(push 'eclim el-get-packages)
(defun tahti-java-mode-init ()
  (require 'eclim)
  (require 'eclimd)
  (setq c-basic-offset 2) ;;set the indentation to 2 spaces
  (setq eclim-auto-save t) ;; autosave is very important
  (global-eclim-mode)
  ;;Treat Java 1.5 @-style annotations as comments
  (setq c-comment-start-regexp "(@|/(/|[*][*]?))")
  (modify-syntax-entry ?@ "< b" java-mode-syntax-table)
  (local-set-key (kbd "RET") 'newline-and-indent)) ; indent on enter
(add-hook 'java-mode-hook 'tahti-java-mode-init)
(provide 'tahti-java-mode)

