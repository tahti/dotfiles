(require 'tahti-keys)
(setq auto-mode-alist (cons '("\\.lzz$" . c-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.lzz$" . whitespace-mode) auto-mode-alist))
(provide 'tahti-c-mode)

