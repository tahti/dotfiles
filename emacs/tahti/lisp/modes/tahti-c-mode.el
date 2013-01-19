(require 'tahti-keys)
(setq auto-mode-alist (cons '("\\.lzz$" . c-mode) auto-mode-alist))
(add-hook 'c-mode 'whitespace-mode)
(provide 'tahti-c-mode)

