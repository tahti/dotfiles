(require 'tahti-keys)
(push 'projectile el-get-packages)
(push 'helm-projectile el-get-packages)

(defun tahti-after-projectile()
  (projectile-global-mode)
  (setq projectile-completion-system 'helm)
  (helm-projectile-on)
)

(provide 'tahti-projectile)
