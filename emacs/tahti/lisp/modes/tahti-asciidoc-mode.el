(push 'doc-mode el-get-packages)
(push 'asciidoc el-get-packages)
(autoload 'doc-mode "doc-mode" nil t)
;(add-to-list 'auto-mode-alist '("\\.proto\\'" . protobuf-mode))
(add-to-list 'auto-mode-alist '("\\.txt$" . doc-mode))
(add-to-list 'auto-mode-alist '("\\.adoc$" . doc-mode))
  (add-hook 'doc-mode-hook
      '(lambda ()
         (turn-on-auto-fill)
         (require 'asciidoc)))

(provide 'tahti-asciidoc-mode)

