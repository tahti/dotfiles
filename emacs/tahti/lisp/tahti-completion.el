(push 'pos-tip el-get-packages)
(push 'popup el-get-packages)

;;{{{ Autocomplate
(push 'auto-complete el-get-packages)
;(push 'auto-complete-yasnippet el-get-packages)
(defun tahti-after-auto-complete ()
  (require 'auto-complete)
  (require 'auto-complete-config)
  ;(require 'auto-complete-yasnippet)
  (ac-config-default)
  ;; add the emacs-eclim source
  ;(require 'ac-emacs-eclim-source)
  ;(ac-emacs-eclim-config)
)


;;{{{ Hippie Exapnd
(setq hippie-expand-try-functions-list
      '(
        yas-hippie-try-expand 
        try-expand-dabbrev
        try-expand-dabbrev-visible
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol
        try-expand-list))


(defun tahti-hippie-expand-completions (&optional hippie-expand-function)
  "Return the full list of possible completions generated by `hippie-expand'.
The optional argument can be generated with `make-hippie-expand-function'."
  (let ((this-command 'tahti-hippie-expand-completions)
        (last-command last-command)
        (buffer-modified (buffer-modified-p))
        (hippie-expand-function (or hippie-expand-function 'hippie-expand)))
    (cl-flet ((ding)) ; avoid the (ding) when hippie-expand exhausts its options.
      (while (progn
               (funcall hippie-expand-function nil)
               (setq last-command 'tahti-hippie-expand-completions)
               (not (equal he-num -1)))))
    ;; Evaluating the completions modifies the buffer, however we will finish
    ;; up in the same state that we began, and (save-current-buffer) seems a
    ;; bit heavyweight in the circumstances.
    (set-buffer-modified-p buffer-modified)
    ;; Provide the options in the order in which they are normally generated.
    (delete he-search-string (reverse he-tried-table))))

;;}}}

(provide 'tahti-completion)
