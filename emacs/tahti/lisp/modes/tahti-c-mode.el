(push 'irony-mode el-get-packages)
(require 'tahti-keys)
(add-hook 'c++-mode-hook 'whitespace-mode)
(add-hook 'c-mode-hook 'whitespace-mode)
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

(defun tahti-irony-mode-init ()
;; replace the `completion-at-point' and `complete-symbol' bindings in
;; irony-mode's buffers by irony-mode's function    "function that runs when csharp-mode is initialized for a buffer."
  (require 'yasnippet)
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async)
)
(setq auto-mode-alist (cons '("\\.lzz$" . c-mode) auto-mode-alist))
(provide 'tahti-c-mode)

