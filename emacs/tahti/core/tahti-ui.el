;; enable subword-mode that lets you move by camelCase
(global-subword-mode 1)

;; point not keep screen position while scrolling
(setq scroll-preserve-screen-position nil)
(scroll-bar-mode -1)

;; use utf-8 environment as default
(set-language-environment 'UTF-8)
(set-locale-environment "UTF-8")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; disable alarm bell and visual bell
(setq visible-bell nil)
(setq ring-bell-function 'ignore)

;; Always rescan buffer for imenu
(set-default 'imenu-auto-rescan t)

(add-to-list 'ido-ignore-directories "target")
(add-to-list 'ido-ignore-directories "node_modules")

;; indentation and tab stops
(setq-default fill-column '80)
(auto-fill-mode 't)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

