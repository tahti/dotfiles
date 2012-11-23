(defvar tahti-tips
  '("Press <C-c o> to open a file with external program."
    "Press <C-c p f> to navigate a project's files with ido."
    "Press <C-c h> to navigate a project in Helm."
    "Press <C-x => to display info about character"
    "Press <C-c r> to rename the current buffer and file it's visiting."
    "Press <C-c t> to open a terminal in Emacs."
    "Explore the Prelude menu to find out about some of Prelude extensions to Emacs."
    "Access the official Emacs manual by pressing <C-h r>."
    "Visit WikEmacs at http://wikemacs.org to find out even more about Emacs."))

(defun tahti-tip-of-the-day ()
  (interactive)
  (message
   (concat "The tip: " (nth (random (length prelude-tips)) tahti-tips))))

(provide 'tahti-tips)

