(require 'tahti-util)
(require 'tahti-helm)
(require 'tahti-windowing)
(require 'tahti-evil)
(require 'tahti-autoloads)
;; use shift + arrow keys to switch between visible buffers
(require 'windmove)
(windmove-default-keybindings)

(define-key yas-minor-mode-map "\C-c&" nil)
;; A complementary binding to the apropos-command (C-h a)
(define-key 'help-command "A" 'apropos)

(global-set-key (kbd "C-x g") 'magit-status)

(fill-keymap 'global
  "C-+" 'text-scale-increase  ;;increase font
  "C--" 'text-scale-decrease  ;;decrease font
  )


(provide 'tahti-keys)

