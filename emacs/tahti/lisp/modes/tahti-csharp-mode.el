(push 'csharp-mode el-get-packages)
(defun tahti-csharp-mode-init ()
     "function that runs when csharp-mode is initialized for a buffer."
     ;(turn-on-auto-revert-mode)
     ;(setq indent-tabs-mode nil)
     ;(require 'flymake)
     ;(flymake-mode 1)
     (require 'yasnippet)
     ;(yas/minor-mode-on)
)

(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)

(add-hook 'csharp-mode-hook 'tahti-csharp-mode-init)
;; setup files ending in “.jjt” to open in javacc-mode
(add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-mode))
(provide 'tahti-csharp-mode)

