(push 'protobuf-mode el-get-packages)
(defun tahti-protobuf-mode-init ()
  (require 'protobuf-mode)
)
(defconst my-protobuf-style
  '((c-basic-offset . 2)
    (indent-tabs-mode . nil)))

(add-hook 'protobuf-mode-hook
  (lambda () (c-add-style "my-style" my-protobuf-style t)))

(add-hook 'protobuf-mode-hook 'tahti-protobuf-mode-init)
(add-to-list 'auto-mode-alist '("\\.proto\\'" . protobuf-mode))

(provide 'tahti-proto-mode)

