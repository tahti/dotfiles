(require 'tahti-dirs)
;;; saveplace ========================================
(setq-default save-place t)
(require 'saveplace)
(setq desktop-save 'if-exists)
;(desktop-save-mode 1)

(defun emacs-process-p (pid)
  "Returns t if command of pid is emacs.
Also returns nil if pid is nil."
  (when pid
    (let* ((attributes (process-attributes pid))
           (comm (cdr (assq 'comm attributes))))
      (or (string= "emacs" comm)
         (string= "emacs.exe" comm)))))

(defadvice desktop-owner (after pry-from-cold-dead-hands activate)
  "Don't allow dead emacsen to own the desktop file."
  (when (not (emacs-process-p ad-return-value))
    (setq ad-return-value nil)))

(require-and-exec 'desktop
  (require 'desktop-menu))

;; File/path settings are set up in in ~/.emacs.d/tahti-dirs.el

(setq desktop-restore-eager nil
      ;; default \ filename history
      desktop-globals-to-save '(desktop-missing-file-warning
                                tags-file-name
                                tags-table-list
                                search-ring
                                regexp-search-ring
                                register-alist))
(provide 'tahti-desktop)


