(push 'python-mode el-get-packages)
(push 'pretty-lambdada el-get-packages)
(push 'pylookup el-get-packages)
(push 'autopep8 el-get-packages)
(push 'py-autopep8 el-get-packages)
(put 'test-case-name 'safe-local-variable (lambda (xx) t)) ;allow any value for test-case-name

(defun tahti-python-mode-init ()
  (require 'ipython)
  (require 'pretty-lambdada)
  (require 'pylookup)
  (require 'py-autopep8)
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  (add-hook 'before-save-hook 'py-autopep8-before-save)
  )

(add-hook 'python-mode-hook 'tahti-python-mode-init)
(add-hook 'python-mode-hook 'turn-on-pretty-lambda-mode)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(provide 'tahti-python-mode)

