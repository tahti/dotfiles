;(require 'tahti-keys)
(push 'cmake-mode el-get-packages)
(push 'andersl-cmake-font-lock el-get-packages)

(defun tahti-cmake-mode-init ()
  (autoload 'andersl-cmake-font-lock-activate "andersl-cmake-font-lock" nil t)
)

(add-hook 'cmake-mode-hook 'tahti-cmake-mode-init)
(add-hook 'cmake-mode-hook 'andersl-cmake-font-lock-activate)

(provide 'tahti-cmake-mode)

