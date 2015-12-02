;(require 'tahti-keys)
(push 'cmake-mode el-get-packages)
(push 'andersl-cmake-font-lock el-get-packages)

(require 'cmake-mode)
;(require 'andersl-cmake-font-lock)
(autoload 'cmake-font-lock-activate "cmake-font-lock" nil t)

;(defun tahti-cmake-mode-init ()
  ;(autoload 'andersl-cmake-font-lock-activate "andersl-cmake-font-lock" nil t)
;)

(add-to-list 'auto-mode-alist '("CMakeLists\\.txt$" . cmake-mode ))
(add-to-list 'auto-mode-alist '("\\.cmake$" . cmake-mode ))
;(add-hook 'cmake-mode-hook 'tahti-cmake-mode-init)
(add-hook 'cmake-mode-hook 'cmake-font-lock-activate)

(provide 'tahti-cmake-mode)

