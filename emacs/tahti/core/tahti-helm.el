(require tahti-config-dirs)
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

(setq helm-idle-delay 0.3
      helm-input-idle-delay 0
      helm-quick-update t
      helm-candidate-number-limit nil
      helm-su-or-sudo "sudo"
      helm-allow-skipping-current-buffer nil
      helm-enable-shortcuts t)
;;if show only the basename in helm-find-files
(setq helm-ff-transformer-show-only-basename nil)

(setq helm-M-x-requires-pattern 0)

(on-full-instance
 (run-with-timer 10 1800  #'start-process "updatedb" "*updatedb*" 
                                          "updatedb" "-U" (expand-file-name "~")
                                                     "-o" tahti-locate-filename
                                                     "-l" "no" 
                                                     "--prunepaths" (expand-file-name "~/workspace/.metadata/ ~/.Private/")))

(setq helm-c-locate-command (format "locate -d %s -i -r %%s" tahti-locate-filename))

(setq helm-c-default-info-index-list '("ansicl" "elisp" "cl" "org" "gnus" "tramp"
                                       "zsh" "coreutils" "find" "libc"
                                       "make" "emacs" "eieio" "latex2e"
                                       "gawk" "sed" "wget" "binutils" "ld"
                                       "grep" "gzip" "libtool"
                                       "texinfo" "info" "gdb"
                                       "sphinx" "python"))
(require 'lacarte)
;; --------------------------------------------------
(require-and-exec 'helm
  (require 'helm-config)
  (require 'helm-misc)
  (require 'helm-match-plugin)

  (helm-mode 1)
  ;; helm for ffap behaves broken
  (push  '(find-file-at-point . ido-completing-read) helm-completing-read-handlers-alist)
  (tahti/set-key 'global "M-x" 'helm-M-x)

  ;; From browse-kill-ring.el
  (defadvice yank-pop (around kill-ring-browse-maybe (arg) activate)
    "If last action was not a yank, run `browse-kill-ring' instead."
    ;; yank-pop has an (interactive "*p") form which does not allow
    ;; it to run in a read-only buffer.  We want browse-kill-ring to
    ;; be allowed to run in a read only buffer, so we change the
    ;; interactive form here.  In that case, we need to
    ;; barf-if-buffer-read-only if we're going to call yank-pop with
    ;; ad-do-it
    (interactive "p")
    (if (not (eq last-command 'yank))
        (helm-show-kill-ring)
      (barf-if-buffer-read-only)
      ad-do-it))

  (defadvice evil-paste-pop (around evil-browse-kill-ring (arg) activate)
    (interactive "p")
    (if (not (memq last-command '(yank evil-paste-before evil-paste-pop evil-paste-after)))
        (helm-show-kill-ring)
      (barf-if-buffer-read-only)
      ad-do-it))

  (require-and-exec 'helm-descbinds
    (helm-descbinds-install))
  ;; Sources ----------------------------------------
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
  ;; helms ----------------------------------------
  (defun tahti/helm-buffers ()
    (interactive)
    ;(funcall (if (string= (frame-parameter nil 'name) "ERC")
                 ;'tahti/erc-buffer
               ;'helm-buffers-list)))
    (funcall ( helm-buffers-list)))

  (defun tahti/helm-files ()
    (interactive)
    (helm :sources '( helm-c-source-recentf
                      helm-c-source-file-cache
                      helm-c-source-files-in-current-dir
                      helm-c-source-files-in-all-dired
                      helm-c-source-locate)
          :buffer "*helm with files*"
          :keymap helm-find-files-map))

  (defvar tahti/helm-uni-sources '())
  (defvar tahti/helm-config-sources '())
  (defun tahti/update-helm-sources ()
    (interactive)
    (setq tahti/helm-config-sources
          `(
            ,(tahti/helm-dir-flat "Emacs" tahti-config-dir t ".el$")
            ,(tahti/helm-dir-deep "Snippets" tahti-snippets-dir t)
            ,(tahti/helm-dir-deep "Zsh" "~/svn/dotfiles/zsh/" t)
            ((name . "Dot")
             (candidates . ,(append (ls-files "~/svn/dotfiles/" t)
                                    (ls-files "~/svn/dotfiles/bin" t)))
             (action . (("Open" . find-file)))
             (type . file))
            ))
    )

  (run-with-timer 0 1200 #'tahti/update-helm-sources)

  (defvar helm-makefile-path nil)
  (defvar helm-makefile-targets
    `((name . "Make")
      (init . (lambda () (setq helm-makefile-path (find-makefile default-directory))))
      (candidates . (lambda () (makefile-targets (concat helm-makefile-path "Makefile"))))
      (volatile)
      (action . (("Make target" . (lambda (candidate)
                                    (compile (concat "cd " helm-makefile-path
                                                     " && make " candidate))))))))

  (defun tahti/helm-config ()
    (interactive)
    (helm :sources tahti/helm-config-sources
          :buffer "*helm config*"
          :keymap helm-find-files-map))
  (defun tahti/helm-make ()
    (interactive)
    (helm :sources helm-makefile-targets
          :buffer "*helm make*"))

  (defun tahti/helm-lacarte ()
    (interactive)
    (helm :sources helm-c-source-lacarte
          :buffer "*helm lacarte*"))
  (tahti/set-key 'global "<f10>" 'tahti/helm-lacarte)

  (defun tahti/helm-flyspell-correct ()
    "Use helm for flyspell correction.
Adapted from `flyspell-correct-word-before-point'."
    (interactive)
    ;; use the correct dictionary
    (flyspell-accept-buffer-local-defs)
    (let ((cursor-location (point))
          (word (flyspell-get-word))
          (opoint (point)))
      (if (consp word)
          (let ((start (car (cdr word)))
                (end (car (cdr (cdr word))))
                (word (car word))
                poss ispell-filter)
            ;; now check spelling of word.
            (ispell-send-string "%\n")	;put in verbose mode
            (ispell-send-string (concat "^" word "\n"))
            ;; wait until ispell has processed word
            (while (progn
                     (accept-process-output ispell-process)
                     (not (string= "" (car ispell-filter)))))
            ;; Remove leading empty element
            (setq ispell-filter (cdr ispell-filter))
            ;; ispell process should return something after word is sent.
            ;; Tag word as valid (i.e., skip) otherwise
            (or ispell-filter
               (setq ispell-filter '(*)))
            (if (consp ispell-filter)
                (setq poss (ispell-parse-output (car ispell-filter))))
            (cond
             ((or (eq poss t) (stringp poss))
              ;; don't correct word
              t)
             ((null poss)
              ;; ispell error
              (error "Ispell: error in Ispell process"))
             (t
              ;; The word is incorrect, we have to propose a replacement.
              (flyspell-do-correct (helm-comp-read "Correction: "
                                                   (append
                                                    (third poss)
                                                    '(("Save word"        . save)
                                                      ("Accept (session)" . session)
                                                      ("Accept (buffer)"  . buffer)))
                                                   :name (format "%s [%s]" word (or ispell-local-dictionary
                                                                                   ispell-dictionary
                                                                                   "Default"))
                                                   :must-match t)

                                   poss word cursor-location start end opoint)))
            (ispell-pdict-save t)))))
  )
(provide 'tahti-helm)

