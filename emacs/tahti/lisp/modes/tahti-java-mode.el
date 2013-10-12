(push 'eclim el-get-packages)
(defun tahti-java-mode-init ()
  (require 'eclim)
  (require 'eclimd)
  (setq c-basic-offset 2) ;;set the indentation to 2 spaces
  (setq eclim-auto-save t) ;; autosave is very important
  (global-eclim-mode)
  ;Displaying compilation error messages in the echo area 
  (setq help-at-pt-display-when-idle t)
  (setq help-at-pt-timer-delay 0.2)
  (help-at-pt-set-timer)
  ;;Treat Java 1.5 @-style annotations as comments
  (setq c-comment-start-regexp "(@|/(/|[*][*]?))")
  (modify-syntax-entry ?@ "< b" java-mode-syntax-table)
  (local-set-key (kbd "RET") 'newline-and-indent)) ; indent on enter

(defun tahti-problems-open ()
  "Opens a new (emacs) window inside the current frame showing the current project compilation problems"
  (interactive)
  (let ((w (selected-window)))
    (select-window (split-window nil (round (* (window-height w) 0.75)) nil))
    (eclim-problems)
    ))

(defun tahti-javadoc()
  (interactive)
  (eclim-java-show-documentation-for-current-element)
  (evil-emacs-state)
)

(add-hook 'java-mode-hook 'tahti-java-mode-init)
(provide 'tahti-java-mode)

