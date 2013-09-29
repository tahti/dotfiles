;documentation for helm - https://github.com/emacs-helm/helm/wiki
;; (require 'tahti-dirs)
;; (require 'tahti-util)

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

;;; Libraries
(push 'sunrise-commander el-get-packages)


(defun tahti-after-sr ()
  (require 'sunrise-commander)
)
(provide 'tahti-sr)
