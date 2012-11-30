;;; tahti-init.el --- Init function. The start entry.

;;; eval me to compile the dir
;; (byte-recompile-directory "~/.emacs.d/lisp" 0 nil)
(defcustom tahti-blacklist
  nil
  "Files in black list are not loaded"
  :group 'tahti-config
  :type '(repeat symbol))


;; locate this file

(eval-and-compile
  (progn
    (require 'cl)
    (load "cl-seq")))
(require 'tahti-dirs)
(add-to-list 'load-path (expand-file-name  "modes" tahti-lisp-dir))
(add-to-list 'load-path tahti-lisp-dir)
(add-to-list 'load-path (expand-file-name  "el-get/el-get" tahti-config-dir))

;; remove system org
(delete-if (lambda (path) (string= "org" (file-name-nondirectory path))) load-path)

;; config changes made through the customize UI will be store here
(setq custom-file (expand-file-name "custom.el" tahti-config-dir))
(setq secrets-file (concat tahti-config-dir "secrets.el"))
(defvar el-get-packages nil)

;; Once user has customized and saved, use user custom file.
(when (file-exists-p custom-file)
  (load custom-file t t))

(when (file-exists-p secrets-file)
  (load secrets-file t t))

(defun tahti-init-load-module (feature)
  (message "[tahti-init] load %s" feature)
  (require feature))

(defun tahti-init-load-modules (&optional before-modules after-modules)
  "Emacs load modules"
  (dolist (feature before-modules)
    (tahti-init-load-module feature))
  ;; load modules in lisp directory
  (dolist (file (nconc (file-expand-wildcards (concat tahti-lisp-dir "tahti-*.el"))
                       (file-expand-wildcards (concat tahti-lisp-dir "modes/tahti-*.el"))))
    (let ((feature (file-name-nondirectory (file-name-sans-extension file)))
          (exclude (append '(tahti-init) before-modules after-modules)))
      (if (memq (intern feature) tahti-blacklist)
          (message "[tahti-init] %s is in black list" feature)
        (unless (memq (intern feature) exclude)
          (tahti-init-load-module (intern feature))))))
  (dolist (feature after-modules)
    (tahti-init-load-module feature)))

(defun tahti-run-after-modules (modules)
  "Looks for funcitons named \"tahti-after-*\" for each module name. If funciton is found it is executed."
  (dolist (module-name modules)
    (let* ((name (symbol-name module-name))
           (func (intern (concat "tahti-after-" name ))))
      (when (fboundp func)
          (message "[tahti-init] run tahti-after-%s" name) 
          (funcall func)))))

(defun tahti-init ()
  "Emacs start entry"
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (add-to-list 'package-archives
             '("marmelade" . "http://marmalade-repo.org/packages/") t)

  (tahti-init-load-modules '(tahti-cedet tahti-theme) '(tahti-el-get))
  (el-get 'sync (reverse el-get-packages))
  (tahti-run-after-modules (reverse el-get-packages))
)

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))

(tahti-init)

(provide 'tahti-init)

