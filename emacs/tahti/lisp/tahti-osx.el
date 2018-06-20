
  (defun tahti-swap-meta-and-super ()
    "Swap the mapping of meta and super. Very useful for people using their Mac
  with a Windows external keyboard from time to time."
    (interactive)
    (if (eq mac-command-modifier 'super)
        (progn
          (setq mac-command-modifier 'meta)
          (setq mac-option-modifier 'super)
          (message "Command is now bound to META and Option is bound to SUPER."))
      (progn
        (setq mac-command-modifier 'super)
        (setq mac-option-modifier 'meta)
        (message "Command is now bound to SUPER and Option is bound to META."))))
  ;(define-key tahti-mode-map (kbd "C-c w") 'tahti-swap-meta-and-super)

  ;; Move to trash when deleting stuff
  (setq delete-by-moving-to-trash t
        trash-directory "~/.Trash/emacs")

  ;; Don't open files from the workspace in a new frame
  (setq ns-pop-up-frames nil)

  (normal-erase-is-backspace-mode t)

  ;; PeepOpen
  ;; See https://gist.github.com/1505658
  ;; if PeepOpen opens selected files in a new Emacs instance
  ;(require 'eproject-peepopen)
)
(provide 'tahti-osx)

