(defvar tahti-tips
  '(
    "Press <C-x C-h> see keys starting with C-x"
    "Press <C-c C-h> see keys starting with C-c"
    "Press <, c SPC> to toggle comment of a region or a line"
    "Press <C-x => to display info about character"
    "Explore the Prelude menu to find out about some of Prelude extensions to Emacs."
    "Access the official Emacs manual by pressing <C-h r>."))

(defun tahti-tip-of-the-day ()
  (interactive)
  (message
   (concat "The tip: " (nth (random (length tahti-tips)) tahti-tips))))

(defun tahti-eval-after-init (form)
  "Add `(lambda () FORM)' to `after-init-hook'.

    If Emacs has already finished initialization, also eval FORM immediately."
  (let ((func (list 'lambda nil form)))
    (add-hook 'after-init-hook func)
    (when after-init-time
      (eval form))))

(tahti-eval-after-init (run-at-time 10 nil 'tahti-tip-of-the-day))

(provide 'tahti-tips)

