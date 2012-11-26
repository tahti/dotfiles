(when (eq system-type 'windows-nt)
  ;; windows friendly font
  ;; Anonymous Pro-10
  ;; Cousine-9
  (set-face-attribute 'default nil :font "Cousine-9")

  ;; set cygwin bash as default shell
  (setq explicit-bash-args '("--login" "-i"))
  (defun cygwin-shell ()
    "Run cygwin bash in shell mode."
    (interactive)
    (let ((explicit-shell-file-name "C:/cygwin/bin/bash"))
      (call-interactively 'shell)))
)
(provide 'tahti-cygwin)

