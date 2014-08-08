(push 'js3-mode el-get-packages)



(defun tahti-javascript-mode-init ()
  (custom-set-variables
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
   '(js3-indent-dots t)
   '(js3-lazy-dots t);
   '(js3-lazy-commas t)
   '(js3-lazy-semicolons t)
   '(js3-lazy-operators t)
   '(js3-expr-indent-offset 2)
   '(js3-paren-indent-offset 2)
   '(js3-square-indent-offset 2)
   '(js3-curly-indent-offset 2))

  (require 'js3-mode)

  ;Displaying compilation error messages in the echo area 
  (setq help-at-pt-display-when-idle t)
  (setq help-at-pt-timer-delay 0.2)
  (help-at-pt-set-timer)
  ;(local-set-key (kbd "RET") 'newline-and-indent) ; indent on enter
)

(add-hook 'j3-mode-hook 'tahti-javascript-mode-init)
;; setup files ending in “.jjt” to open in javacc-mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . js3-mode))
(add-to-list 'auto-mode-alist '("\\.jsm\\'" . js3-mode))
(provide 'tahti-javascript-mode)

