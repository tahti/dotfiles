(push 'eclim el-get-packages)
(push 'javacc-mode el-get-packages)
(defun tahti-java-mode-init ()
  (custom-set-variables
    ;;
    '(eclim-eclipse-dirs '("/opt/eclipse"))
    '(eclim-executable "/opt/eclipse/eclim")
  )
  (setq c-basic-offset 2;;set the indentation to 2 spaces
    tab-width 2
    indent-tabs-mode nil)
  (require 'eclim)
  (require 'eclimd)
  (setq eclim-auto-save t) ;; autosave is very important
  (eclim-mode 1)
  ;(if (null (get-buffer-process eclimd-process-buffer));eclimd not running
    ;(start-eclimd "/home/piotr/workspace"))
  (global-eclim-mode)

  ;Displaying compilation error messages in the echo area 
  (setq help-at-pt-display-when-idle t)
  (setq help-at-pt-timer-delay 0.2)
  (help-at-pt-set-timer)
  ;;Treat Java 1.5 @-style annotations as comments
  (setq c-comment-start-regexp "(@|/(/|[*][*]?))")
  (modify-syntax-entry ?@ "< b" java-mode-syntax-table)
  (local-set-key (kbd "RET") 'newline-and-indent) ; indent on enter
)

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
(add-hook 'java-mode-hook 'whitespace-mode)
;; setup files ending in “.jjt” to open in javacc-mode
(add-to-list 'auto-mode-alist '("\\.jjt\\'" . javacc-mode))
(provide 'tahti-java-mode)

