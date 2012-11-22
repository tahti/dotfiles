;; Emacs users obviously have little need for Command and Option keys,
;; but they do need Meta and Super
(setq mac-command-modifier 'super)
(setq mac-option-modifier 'meta)
(setq ns-function-modifier 'hyper)

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
(define-key tahti-mode-map (kbd "C-c w") 'tahti-swap-meta-and-super)

;; mac friendly font
;; Menlo-12
(set-face-attribute 'default nil :font "Cousine-12")

;; make sure path is correct when launched as application
(setenv "PATH" (concat "/usr/local/share/python:/usr/local/bin:/usr/local/sbin:" (getenv "PATH")))
(push "/usr/local/bin" exec-path)
(push "/usr/local/sbin" exec-path)
(push "/usr/local/share/python" exec-path)

;; keybinding to toggle full screen mode
(global-set-key (quote [M-f10]) (quote ns-toggle-fullscreen))

;; Move to trash when deleting stuff
(setq delete-by-moving-to-trash t
      trash-directory "~/.Trash/emacs")

;; Ignore .DS_Store files with ido mode
(add-to-list 'ido-ignore-files "\\.DS_Store")

;; Don't open files from the workspace in a new frame
(setq ns-pop-up-frames nil)

(normal-erase-is-backspace-mode t)

;; PeepOpen
;; See https://gist.github.com/1505658
;; if PeepOpen opens selected files in a new Emacs instance
;(require 'eproject-peepopen)

(provide 'tahti-osx)

