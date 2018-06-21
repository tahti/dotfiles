;documentation for helm - https://github.com/emacs-helm/helm/wiki
;; (eval-when-compile (require 'cl))
 ;(run-with-timer 10 1800  #'start-process "updatedb" "*updatedb*"
                                          ;"updatedb" "-U" (expand-file-name "~")
                                                     ;"-o" tahti-locate-file
                                                     ;"-l" "no"
                                                     ;"--prunepaths" (expand-file-name "~/workspace/.metadata/ ~/.Private/"))


;; (custom-set-variables
;;  '(helm-c-adaptive-history-file (expand-file-name "helm-c-adaptive-history" tahti-var-dir ))
;;  '(helm-idle-delay 0.3)
;;  '(helm-input-idle-delay 0)
;;  '(helm-quick-update t)
;;  '(helm-candidate-number-limit 99)
;;  '(helm-su-or-sudo "sudo")
;;  '(helm-ff-transformer-show-only-basename nil)
;;  '(helm-M-x-requires-pattern 0)
;;  )
;; disable mouse
(setq sr-cursor-follows-mouse nil)
;; replace dired
(setq find-directory-functions (cons 'sr-dired find-directory-functions))

(defun tahti-enter-fullscreen ()
  "Enter fullscreen" 
  (interactive)
  (if (not (frame-parameter nil 'fullscreen))
    (toggle-frame-fullscreen))
)
(defun tahti-leave-fullscreen ()
  "Leave fullscreen"
  (interactive)
  (if (frame-parameter nil 'fullscreen)
    (toggle-frame-fullscreen))
)
(defun tahti-toggle-fullscreen ()
  "Leave fullscreen by sending message to i3"
  (interactive)
  (toggle-frame-fullscreen)
)
(defun tahti-inplace-copy ()
  "Copy file in place"
  (interactive)
  (sr-inplace-do #'copy-file "Copy in place to")
)

(defun tahti-inplace-rename ()
  "Rename file in place"
  (interactive)
  (sr-inplace-do #'rename-file "Rename in place to")
)
;;; Libraries
(push 'sunrise-commander el-get-packages)

(advice-add 'sr-quit :after #'tahti-leave-fullscreen)

(defun tahti-after-sr ()
  (require 'sunrise-commander)
)
(provide 'tahti-sr)

