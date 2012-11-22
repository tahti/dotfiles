(require 'cl)
(require 'tahti-utils)
(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("marmelade" . "http://marmalade-repo.org/packages/") t)

(load-file "~/.emacs.d/vendor/cedet/cedet-devel-load.el")
(add-to-list 'load-path "~/.emacs.d/vendor/cedet/contrib/")
(add-to-list 'Info-directory-list "~/.emacs.d/vendor/cedet/doc/info")

(defvar tahti/hostname (car (split-string system-name "\\." t)))

(package-initialize)
;; required because of a package.el bug
(setq url-http-attempt-keepalives nil)

(defvar tahti-packages
        '(
           ;ack-and-a-half 
           ;exec-path-from-shell 
           ;expand-region
           evil
           evil-numbers
           evil-leader
           ;gist 
           helm helm-projectile 
           magit 
           ;magithub 
           markdown-mode
           melpa
           rainbow-mode 
           surround
           ;volatile-highlights 
           yasnippet 
         )
         "A list of packages to ensure are installed at launch.")

(defun tahti-packages-installed-p ()
  (loop for p in tahti-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(defun tahti-install-packages ()
  (unless (tahti-packages-installed-p)
    ;; check for new packages (package versions)
    (message "%s" "Emacs is now refreshing its package database...")
    (package-refresh-contents)
    (message "%s" " done.")
    ;; install the missing packages
    (dolist (p tahti-packages)
      (unless (package-installed-p p)
        (package-install p)))))

(tahti-install-packages)

(defmacro tahti-auto-install (extension package mode)
  `(add-to-list 'auto-mode-alist
                `(,extension . (lambda ()
                                 (unless (package-installed-p ',package)
                                   (package-install ',package))
                                 (,mode)))))

(when (package-installed-p 'markdown-mode)
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))

(defvar tahti/standard-settings '(
                                 tahti-color
                                 ;tahti-autoloads
                                 ;tahti-helm
                                 ;tahti-buffer
                                 ;tahti-calendar
                                 ;tahti-desktop
                                 ;tahti-ediff
                                 tahti-evil
                                 ;tahti-elisp
                                 ;tahti-erc
                                 ;tahti-files
                                 ;tahti-keys
                                 ;tahti-mail
                                 ;tahti-project
                                 tahti-snippets
                                 ;tahti-ui
                                 ;tahti-modeline
                                 ;tahti-org
                                 ;tahti-vcs
                                 ;tahti-shell
                                 ;tahti-workgroups
                                 ;tahti-write
                                 ;tahti-completion
                                 ))

(mapc #'require tahti/standard-settings)

(provide 'tahti-packages)

