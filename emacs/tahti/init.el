;;; init.el --- configuration entry point.
;;
;; Copyright (c) 2012 Piotr Kordy
;;
;; This file is not part of GNU Emacs.
;;

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(message "Evil emacs is powering up... Be patient, Master %s!" (getenv "USER"))
(setq load-path (cons (concat user-emacs-directory "lisp") load-path))
(require 'tahti-init)
(add-hook 'emacs-startup-hook (lambda ()
                                (message "Time needed to load: %s seconds."
                                         (emacs-uptime "%s"))))
(defun byte-compile-config-on-save ()
  "Compile elisp files in the emacs.d dir unless they are themes."
  (let ((fname (buffer-file-name)))
    (when (string-match "emacs\\.d/lisp/.*\\.el$" fname)
      (byte-compile-file fname))))

(add-hook 'after-save-hook 'byte-compile-config-on-save)
;;; init.el ends here
