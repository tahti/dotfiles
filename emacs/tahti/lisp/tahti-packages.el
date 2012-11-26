;(require 'cl)
;(require 'package)
;(require 'tahti-cedet) ; we need to load cedet first

;(add-to-list 'package-archives
             ;'("melpa" . "http://melpa.milkbox.net/packages/") t)
;(add-to-list 'package-archives
             ;'("marmelade" . "http://marmalade-repo.org/packages/") t)

;(defun add-to-load-path (&rest dirs)
  ;(dolist (dir dirs load-path)
    ;(add-to-list 'load-path (expand-file-name dir) nil #'string=)))

;(add-to-load-path (expand-file-name "vimvars" tahti-vendor-dir)
                  ;(expand-file-name "desktop-menu" tahti-vendor-dir)
                  ;(expand-file-name "help-mode-plus" tahti-vendor-dir)
                  ;(expand-file-name "help-fns-plus" tahti-vendor-dir)
                  ;(expand-file-name "help-plus" tahti-vendor-dir)
                  ;(expand-file-name "menu-bar-plus" tahti-vendor-dir)
;)

;(defvar tahti/hostname (car (split-string system-name "\\." t)))

;(package-initialize)
;; required because of a package.el bug
;(setq url-http-attempt-keepalives nil)

;(defvar tahti-packages
        ;'(
           ;;ack-and-a-half 
           ;;exec-path-from-shell 
           ;;expand-region
           ;;
           ;evil
           ;evil-numbers
           ;evil-leader
           ;;gist 
           ;;helm
           ;magit 
           ;;magithub 
           ;markdown-mode
           ;melpa
           ;lacarte
           ;queue
           ;rainbow-mode 
           ;surround
           ;;volatile-highlights 
           ;yasnippet 
           ;w3m
         ;)
         ;"A list of packages to ensure are installed at launch.")

;(defun tahti-packages-installed-p ()
  ;(loop for p in tahti-packages
        ;when (not (package-installed-p p)) do (return nil)
        ;finally (return t)))

;(defun tahti-install-packages ()
  ;(unless (tahti-packages-installed-p)
    ;;; check for new packages (package versions)
    ;(message "%s" "Emacs is now refreshing its package database...")
    ;(package-refresh-contents)
    ;(message "%s" " done.")
    ;;; install the missing packages
    ;(dolist (p tahti-packages)
      ;(unless (package-installed-p p)
        ;(package-install p)))))

;(tahti-install-packages)

;(defmacro tahti-auto-install (extension package mode)
  ;`(add-to-list 'auto-mode-alist
                ;`(,extension . (lambda ()
                                 ;(unless (package-installed-p ',package)
                                   ;(package-install ',package))
                                 ;(,mode)))))

;(when (package-installed-p 'markdown-mode)
  ;(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  ;(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))

;(defvar tahti/standard-settings '(
                                 ;tahti-color
                                 ;;tahti-autoloads
                                 ;tahti-helm
                                 ;tahti-buffer
                                 ;;tahti-calendar
                                 ;tahti-desktop
                                 ;;tahti-ediff
                                 ;tahti-evil
                                 ;;tahti-elisp
                                 ;;tahti-erc
                                 ;;tahti-files
                                 ;;tahti-icicles
                                 ;tahti-keys
                                 ;;tahti-mail
                                 ;;tahti-project
                                 ;tahti-snippets
                                 ;tahti-ui
                                 ;;tahti-modeline
                                 ;;tahti-org
                                 ;;tahti-vcs
                                 ;;tahti-shell
                                 ;;tahti-workgroups
                                 ;;tahti-write
                                 ;))

;(mapc #'require tahti/standard-settings)

(provide 'tahti-packages)

