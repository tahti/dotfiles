;; modeline==========================================
(push 'powerline el-get-packages)

(defface tahti/major-active
  '((t (:foreground "white" :inherit powerline-active1)))
  "Tahti major-mode face 1.")

(defface tahti/modified
  '((t (:background "red" :inherit mode-line-active)))
  "Tahti modified face.")
(defface tahti/inactive2
  '((t (:background "red" :inherit powerline-inactive2)))
  "Tahti face2 inactive.")

(defun not-center-theme ()
  "Setup a mode-line with major and minor modes centered."
  (interactive)
  (setq-default mode-line-format
  '("%e"
    (:eval
     (let* ((active (eq powerline-selected-window (selected-window)))
            (mode-line (if active 'mode-line 'mode-line-inactive))
            (face1 (if active 'powerline-active1 'powerline-inactive1))
            (face2 (if active 'powerline-active2 'powerline-inactive2))
            (major (if active 'tahti/major-active 'powerline-inactive1))
            (lhs (list
                  (powerline-buffer-size nil 'l)
                  (powerline-raw "%b" nil 'l)
                  ;(powerline-buffer-id nil 'l);this sets buffer name on black background
                  " "
                  (if (buffer-modified-p)
                     (propertize "+" 'face 'tahti/modified)
                     "%*")

                  (powerline-raw " ")
                  (powerline-arrow-left nil face1)

                  (powerline-major-mode major 'l)
                  (powerline-minor-modes face1 'l)
                  (powerline-raw mode-line-process face1 'l)

                  (powerline-narrow face1 'l)
                  (powerline-raw " " face1)

                  (powerline-arrow-left face1 face2)

                  (powerline-vc face2)
                  ))
            (rhs (list
                  (powerline-raw global-mode-string face2 'r)

                  (powerline-arrow-right face2 face1)

                  (powerline-raw "%4l" face1 'r)
                  (powerline-raw ":" face1)
                  (powerline-raw "%3c" face1 'r)
                  evil-mode-line-tag

                  (powerline-arrow-right face1 nil)
                  (powerline-raw " ")

                  (powerline-raw "%6p" nil 'r)
                  (powerline-hud face2 face1))))
       (concat 
        (powerline-render lhs)
        (powerline-fill face2 (powerline-width rhs))
        (powerline-render rhs) )))))
)

(defun tahti-after-powerline ()
  (add-hook 'post-command-hook (lambda ()
                               (when (not (minibuffer-selected-window))
                                 (setq powerline-selected-window (selected-window)))))
  (not-center-theme)
  ;(powerline-default-theme)
  ;(powerline-center-theme)
  ;(powerline-nano-theme)
  ;(powerline-vim-theme)
;;uniquify ==========================================
  (require 'uniquify)
  (setq uniquify-buffer-name-style 'forward
         uniquify-separator "/"
         uniquify-after-kill-buffer-p t       ; rename after killing uniquified 
         uniquify-ignore-buffers-re "^\\*")   ; don't muck with special buffers 
)

(provide 'tahti-mode-line)
;;; tahit-ui.el ends here
