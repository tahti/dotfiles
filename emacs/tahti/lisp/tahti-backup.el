;;; backups & autosave===============================
(custom-set-variables
 '(save-place-file save-place-file) 
 '(auto-save-interval 300)   ;autosave every 300s
 '(auto-save-timeout 20)     ;autosave after 20s idle
 '(delete-auto-save-files t) ;delete autosave when saving
 '(auto-save-file-name-transforms `((".*" ,autosave-directory t))) ;set autosave dir
 '(auto-save-list-file-prefix nil)   ;do not list saved files
 )
;; make a shell script executable automatically on save
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)
;;; saveplace ========================================
;; restore cursor position in file
(setq-default save-place t)
(require 'saveplace)

(provide 'tahti-backup)
