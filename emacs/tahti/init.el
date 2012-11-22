;;; init.el --- configuration entry point.
;;
;; Copyright (c) 2012 Piotr Kordy
;;
;; This file is not part of GNU Emacs.
;;
(message "Evil emacs is powering up... Be patient, Master %s!" (getenv "USER"))
(require 'tahti-dirs)

(unless (file-exists-p tahti-var-dir)
  (make-directory snippets-var-dir))
(unless (file-exists-p tahti-snippets-dir)
  (make-directory tahti-snippets-dir))
(unless (file-exists-p tahti-savefile-dir)
  (make-directory tahti-savefile-dir))

(add-to-list 'load-path tahti-vendor-dir)
(add-to-list 'load-path tahti-config-dir)
(add-to-list 'load-path tahti-core-dir)
(add-to-list 'load-path (expand-file-name  "modes" tahti-config-dir))

;; the core stuff
(require 'tahti-util)
(require 'tahti-packages)
;(require 'tahti-ui)

;; OSX specific settings
(when (eq system-type 'darwin)
  (require 'tahti-osx))
;; Windows specific settings
(when (eq system-type 'windows-nt)
  (require 'tahti-cygwin))

;; config changes made through the customize UI will be store here
(setq custom-file (expand-file-name "custom.el" tahti-personal-dir))

;; load the personal settings (this includes `custom-file')
(when (file-exists-p tahti-personal-dir)
  (mapc 'load (directory-files tahti-personal-dir 't "^[^#].*el$")))

(add-hook 'emacs-startup-hook (lambda ()
                                (message "Time needed to load: %s seconds."
                                         (emacs-uptime "%s")))

(add-hook 'after-save-hook 'byte-compile-config-on-save)

(require 'tahti-tips)
(tahti-eval-after-init
 ;; greet the user with some useful tip
 (run-at-time 5 nil 'tahti-tip-of-the-day))

;;; init.el ends here
