(require 'tahti-keys)
 ;; use utf-8 environment as default dammit
(setq locale-coding-system 'utf-8)
(set-language-environment 'UTF-8)
(set-locale-environment "UTF-8")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
;;;extended help mode ===============================
(push 'info+ el-get-packages)
(push 'help-mode+ el-get-packages)
(push 'help-fns+ el-get-packages)
(push 'help+ el-get-packages)
(defun tahti-after-help+ ()
  (require 'info+)
  (require 'help-mode+)
  (require 'help-fns+)
  (require 'help+)
)
(custom-set-variables
 '(tab-width 2)
 '(indent-tabs-mode nil)
 '(show-paren-mode t)
 '(fill-column 80)
 '(set-mark-command-repeat-pop t)
)
;;; w3m ==============================================
(setq browse-url-browser-function 'w3m-browse-url)
(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
 ;; optional keyboard short-cut
;;; woman - help files ===============================
(setq-default woman-use-own-frame nil
              woman-use-topic-at-point t
              woman-imenu t)
;
;;; backups & autosave===============================
(add-hook 'find-file-hook 'auto-save-mode)
(setq auto-save-timeout 20)
;; make a shell script executable automatically on save
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)
(setq auto-save-file-name-transforms
          `((".*" ,autosave-directory t)))

;;; recent files ===================================
(setq recentf-auto-cleanup 'never)
(setq recentf-max-saved-items 200)
(require-and-exec 'recentf
   (recentf-mode 1)
   (setq recentf-exclude '(
                           "\\.recentf"
                           "\\.ido\\.last"
                           "\\.keychain/.*?-sh\\(-gpg\\)?"
                           ))
   (add-hook 'kill-emacs-hook #'recentf-cleanup)
   )

;;;; vimvars =========================================
(push 'vimvars el-get-packages)
(defun tahti-after-vimvars ()
  (add-hook 'find-file-hook 'vimvars-obey-vim-modeline))
  (tahti-global-keys) ;run at the last package

(provide 'tahti-editor)
;;; backups & autosave===============================
(add-hook 'find-file-hook 'auto-save-mode)
(setq auto-save-timeout 20)
;; make a shell script executable automatically on save
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)
(setq auto-save-file-name-transforms
          `((".*" ,autosave-directory t)))

;;; recent files ===================================
(setq recentf-auto-cleanup 'never)
(setq recentf-max-saved-items 200)
(require-and-exec 'recentf
   (recentf-mode 1)
   (setq recentf-exclude '(
                           "\\.recentf"
                           "\\.ido\\.last"
                           "\\.keychain/.*?-sh\\(-gpg\\)?"
                           ))
   (add-hook 'kill-emacs-hook #'recentf-cleanup)
   )

;;; pretty-mode ======================================
(setq pp^L-^L-string (concat (make-string 30 ? ) "⁂" (make-string 30 ? ))
      pp^L-^L-string-pre "")

(push 'pretty-mode el-get-packages)
  (when (fboundp 'pretty-control-l-mode)
    (pretty-control-l-mode 1))
  (defun tahti-after-pretty-mode ()
    (dolist (mode '(python-mode c-mode java-mode cpp-mode))
            (pretty-add-keywords mode '(("="  . "←")
                                        ("==" . "≡"))))
  (global-pretty-mode 1)
)
;;; ==================================================
;;; scratch ====================
(defun save-a-scratch ()
  "Prevent *scratch* buffer from being killed.
Intended as `kill-buffer-query-functions' fun."
  (not (string= "*scratch*" (buffer-name))))

(push #'save-a-scratch kill-buffer-query-functions)
;;; ==============================
(setq multi-term-dedicated-select-after-open-p t)

(provide 'tahti-editor)
;;; tahit-ui.el ends here

