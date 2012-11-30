;; modeline==========================================
(push 'powerline el-get-packages)

(defface tahti/major-active
  '((t (:foreground "white" :inherit powerline-active1)))
  "Tahti major-mode face 1.")

(defface tahti/modified
  '((t (:background "red" :inherit mode-line-active)))
  "Tahti modified face.")

(defun not-center-theme ()
  (setq-default mode-line-format
  '("%e"
    (:eval
     (let* ((active (eq (frame-selected-window) (selected-window)))
            (face1 (if active 'powerline-active1 'powerline-inactive1))
            (face2 (if active 'powerline-active2 'powerline-inactive2))
            (major (if active 'tahti/major-active 'powerline-inactive1))
            (lhs (list
                  (powerline-buffer-size nil 'l)
                  (powerline-buffer-id nil 'l)
                  " "
                  (if (buffer-modified-p)
                     (propertize "+" 'face 'tahti/modified)
                     "%*")

                  (powerline-raw " ")
                  (powerline-arrow-right nil face1)

                  (powerline-major-mode major 'l)
                  (powerline-minor-modes face1 'l)
                  (powerline-raw mode-line-process face1 'l)

                  (powerline-narrow face1 'l)

                  (powerline-arrow-right face1 face2)

                  (powerline-vc face2)
                  ))
            (rhs (list
                  (powerline-raw global-mode-string face2 'r)

                  (powerline-arrow-left face2 face1)

                  (powerline-raw "%4l" face1 'r)
                  (powerline-raw ":" face1)
                  (powerline-raw "%3c" face1 'r)
                  evil-mode-line-tag

                  (powerline-arrow-left face1 nil)
                  (powerline-raw " ")

                  (powerline-raw "%6p" nil 'r)

                  (powerline-hud face2 face1))))
       (concat 
        (powerline-render lhs)
        (powerline-fill face2 (powerline-width rhs))
        (powerline-render rhs) )))))
)

(defun tahti-after-powerline ()
  (not-center-theme)
  ;(powerline-default)
;;uniquify ==========================================
  (require 'uniquify)
  (setq uniquify-buffer-name-style 'forward
         uniquify-separator "/"
         uniquify-after-kill-buffer-p t       ; rename after killing uniquified 
         uniquify-ignore-buffers-re "^\\*")   ; don't muck with special buffers 
)

(provide 'tahti-mode-line)
;;; tahit-ui.el ends here
