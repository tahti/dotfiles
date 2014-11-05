(push 'irfc el-get-packages)
(defun tahti-after-irfc ()
  (require 'irfc)
  (tahti-irfc-keys)

)

(require 'tahti-keys)
(setq irfc-assoc-mode t) ;load irfc mode automatically

(provide 'tahti-rfc-mode)

