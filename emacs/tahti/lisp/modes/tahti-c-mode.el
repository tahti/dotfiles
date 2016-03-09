(push 'irony-mode el-get-packages)
(push 'irony-eldoc el-get-packages)
(push 'flycheck-irony el-get-packages)
(push 'company-irony el-get-packages)
(require 'tahti-keys)
(require 'cpp-auto-include)

(add-hook 'c++-mode-hook 'whitespace-mode)
(add-hook 'c-mode-hook 'whitespace-mode)
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'c++-mode-hook 'company-mode)
(add-hook 'c-mode-hook 'company-mode)
(add-hook 'objC-mode-hook 'irony-mode)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
(add-hook 'c++-mode-hook 'flycheck-mode)
(add-hook 'c-mode-hook 'flycheck-mode)
(add-hook 'irony-mode-hook 'irony-eldoc)
(eval-after-load 'flycheck
'(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

(defun tahti-irony-mode-init ()
;; replace the `completion-at-point' and `complete-symbol' bindings in
;; irony-mode's buffers by irony-mode's function    "function that runs when csharp-mode is initialized for a buffer."
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async)
  (eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))
  ;; (optional) adds CC special commands to `company-begin-commands' in order to
  ;; trigger completion at interesting places, such as after scope operator
  ;;     std::|
  (require 'yasnippet)
  (tahti-irony-mode-keys)
  (message "irony mode init hook")
)

(setq auto-mode-alist (cons '("\\.lzz$" . c-mode) auto-mode-alist))
(add-hook 'irony-mode-hook 'tahti-irony-mode-init)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)

(provide 'tahti-c-mode)

