;documentation for helm - https://github.com/emacs-helm/helm/wiki
(require 'tahti-dirs)
(require 'tahti-util)

(eval-when-compile (require 'cl))
 ;(run-with-timer 10 1800  #'start-process "updatedb" "*updatedb*"
                                          ;"updatedb" "-U" (expand-file-name "~")
                                                     ;"-o" tahti-locate-file
                                                     ;"-l" "no"
                                                     ;"--prunepaths" (expand-file-name "~/workspace/.metadata/ ~/.Private/"))

(defun f-alt (&rest alternatives)
  "Test functions in `alternatives' and return first bound."
  (catch 'found
    (dolist (f alternatives)
      (if (functionp f)
          (throw 'found f)))))


(custom-set-variables
 '(helm-c-adaptive-history-file (expand-file-name "helm-c-adaptive-history" tahti-var-dir ))
 '(helm-idle-delay 0.3)
 '(helm-input-idle-delay 0)
 '(helm-quick-update t)
 '(helm-candidate-number-limit 99)
 '(helm-su-or-sudo "sudo")
 '(helm-ff-transformer-show-only-basename nil)
 '(helm-M-x-requires-pattern 0)
 )

;;; Libraries
(push 'helm           el-get-packages)
(push 'helm-descbinds el-get-packages)
(push 'lacarte        el-get-packages)


(defun tahti-after-helm ()
  (require 'helm-descbinds)
  (helm-descbinds-mode) ;replace descbinds
  (require 'helm-misc)
  (require 'helm-config)
  (require 'helm-mode)
  (require 'helm-buffers)
  (require 'helm-files)
  ;(require 'helm-locate)
  ;(require 'helm-w3m)
  ;; helm for ffap behaves broken
  (push  '(find-file-at-point . ido-completing-read) helm-completing-read-handlers-alist)
  ;; helm for lacarte is broken
  (push  '(menu-bar-open . nil) helm-completing-read-handlers-alist)
  (push  '(tmm-shortcut . nil) helm-completing-read-handlers-alist)

  (setq helm-c-boring-file-regexp
        (rx (or
             ;; directories
             (and "/"
                (or ".svn" "CVS" "_darcs" ".git" ".hg" "auto" "_region_" ".prv" "__pycache__")
                (or "/" eol))
             ;; files
             (and line-start  (or ".#" "."))
             (and (or ".class" ".la" ".o" "~" ".pyc") eol)))

        helm-c-boring-buffer-regexp
        (rx (or
             (and line-start  " ")
             ;; helm-buffer
             "*helm"
             "*ac-mode-"
             "Map_Sym.txt"
             "*Ibuffer*"
             "*Help*"
             "*Pp Eval Output*"
             "*Completions*"
             "*Customize"
             "*Messages*")))
  ;(setq helm-c-locate-command
        ;(case system-type
          ;('gnu/linux (format "locate -i -d %s -r %%s" tahti-locate-file))
          ;('berkeley-unix "locate -i %s")
          ;('windows-nt "es -i -r %s")
          ;(t "locate %s")))
  ;;; Sources
  (defun tahti/helm-dir-deep (source-name dir &optional dotfiles fmatch dmatch)
    "Returns an helm source for a particular directory."
    `((name . ,(concat source-name))
      (candidates . ,(ls-files-deep dir dotfiles fmatch dmatch))
      (action . (("Open" . find-file)))
      (type . file)))
  (defun tahti/helm-dir-flat (source-name dir &optional dotfiles fmatch dmatch)
    "Returns an helm source for a particular directory."
    `((name . ,(concat source-name))
      (candidates . ,(ls-files-deep-1 dir dotfiles fmatch dmatch))
      (action . (("Open" . find-file)))
      (type . file)))
  (defun tahti/helm-dir (source-name dir &optional dotfiles match)
    "Returns an helm source for a particular directory."
    `((name . ,(concat source-name))
      (candidates . ,(ls-files dir dotfiles match))
      (action . (("Open" . find-file)))
      (type . file)))
  ;; --------------------------------------------------
  ;; disable y-n question for inserting a file 
    (defadvice helm-insert-file(around stfu compile activate)
      (flet ((yes-or-no-p (&rest args) t)
             (y-or-n-p (&rest args) t))
        ad-do-it))




  (helm-mode 1)
  (setq enable-recursive-minibuffers t)
  (defvar tahti/helm-config-sources '())
  (setq helm-ff-auto-update-initial-value nil) ;disable autoupdating when one candidate left
  (defun tahti/update-helm-sources ()
    (interactive)
    (setq tahti/helm-config-sources
          `(
            ,(tahti/helm-dir-flat "Emacs" (file-truename tahti-config-dir)  t ".el$")
            ,(tahti/helm-dir-deep "Snippets" (file-truename tahti-snippets-dir) t)
            ,(tahti/helm-dir-deep "Zsh" (expand-file-name "../../zsh" (file-truename tahti-config-dir)) t)
            ,(tahti/helm-dir-deep "Vim" (expand-file-name "../../vim/" (file-truename tahti-config-dir)) t)
            ,(tahti/helm-dir-deep "Urxvt" (expand-file-name  "../../urxvt/" (file-truename tahti-config-dir)) t)
            ((name . "Dot")
             (candidates . ,(append (ls-files "~/.i3/" t)
                                    (ls-files "~/bin" t)
                                    (ls-files (expand-file-name "../../singles/"(file-truename  tahti-config-dir)) t)))
             (action . (("Open" . find-file)))
             (persistent-action . helm-find-files-persistent-action)
             (type . file))
            ))
    )

  (run-with-timer 0 3600 #'tahti/update-helm-sources)

  ;(setq tahti/file-sources
                  ;`( ,helm-c-source-ffap-line
                     ;,helm-c-source-ffap-guesser
                     ;,helm-c-source-find-files
                     ;,helm-c-source-recentf
                     ;,helm-c-source-file-cache
                     ;,helm-c-source-file-name-history
                     ;,helm-c-source-files-in-all-dired
                     ;))


  (defun tahti/config-files ()
    (interactive)
    (helm :sources tahti/helm-config-sources
          :buffer "*helm config files*"
          :keymap helm-find-files-map))

  (defun tahti/helm-history-files (&optional preselect)
    (interactive)
    (helm-find-files 1))

  (defun tahti/helm-files (&optional preselect)
    (interactive)
    (sunrise-cd))
    ;(helm :sources tahti/file-sources
          ;:prompt "Find Files: "
          ;;:input fname
          ;:preselect preselect
          ;:case-fold-search helm-file-name-case-fold-search
          ;:buffer "*helm with files*"
          ;:keymap helm-find-files-map))


  ;;; ido-mode =========================================
  (ido-mode 'files)
  (ido-everywhere 1)
  (add-to-list 'ido-ignore-directories "target")
  (add-to-list 'ido-ignore-directories "node_modules")
  (setq ido-enable-flex-matching t)

  (defalias 'tahti/file (f-alt 'tahti/helm-files 'ido-find-file))
  (defalias 'tahti/file-alternate (f-alt 'ido-find-file 'tahti/helm-files))
  (defalias 'tahti/buffer (f-alt 'helm-buffers-list 'ido-switch-buffer))
  (defalias 'tahti/buffer-alternate (f-alt 'ido-switch-buffer 'helm-buffers-list))
)

(provide 'tahti-helm)

