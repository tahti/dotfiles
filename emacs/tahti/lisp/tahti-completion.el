(push 'pos-tip el-get-packages)
(push 'popup el-get-packages)

;;{{{ Autocomplate
(push 'auto-complete el-get-packages)
(defun tahti-after-auto-complete ()
  (require 'auto-complete)
)


;;{{{ Hippie Exapnd
(setq hippie-expand-try-functions-list
      '(
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
    (flet ((ding)) ; avoid the (ding) when hippie-expand exhausts its options.
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

(defmacro tahti-ido-hippie-expand-with (hippie-expand-function)
  "Generate an interactively-callable function that offers ido-based completion
using the specified hippie-expand function."
  `(call-interactively
    (lambda (&optional selection)
      (interactive
       (let ((options (tahti-hippie-expand-completions ,hippie-expand-function)))
         (if options
             (list (ido-completing-read "Completions: " options)))))
      (if selection
          (he-substitute-string selection t)
        (message "No expansion found")))))

(defun tahti-ido-hippie-expand ()
  "Offer ido-based completion for the word at point."
  (interactive)
  (tahti-ido-hippie-expand-with 'hippie-expand))
;;}}}

(provide 'tahti-completion)
