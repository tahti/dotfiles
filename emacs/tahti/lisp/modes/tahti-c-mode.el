(require 'tahti-keys)
(add-hook 'c-mode-hook 'whitespace-mode)
(setq auto-mode-alist (cons '("\\.lzz$" . c-mode) auto-mode-alist))
(provide 'tahti-c-mode)

