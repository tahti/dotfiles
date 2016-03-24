(push 'markdown-mode el-get-packages)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(defun tahti-markdown-mode-init ()
  (custom-set-variables
    ;; 
    '(markdown-list-indent-width 2)
    '(markdown-unordered-list-item-prefix "  - ")
  )
  (tahti-markdown-keys)
)

(add-hook 'markdown-mode-hook 'tahti-markdown-mode-init)
(provide 'tahti-markdown-mode)
