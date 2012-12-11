(require 'tahti-dirs)

(push 'cedet el-get-packages)


;(add-to-list 'load-path (expand-file-name "contrib" cedet-root-path))
;(load-file (expand-file-name "cedet-devel-load.el" cedet-root-path))
;(add-to-list 'Info-directory-list(expand-file-name "doc/info" cedet-root-path))
(defun tahti-after-cedet ()
  (setq cedet-root-path (el-get-package-directory "cedet"))
  (unless (file-exists-p (expand-file-name "lisp/cedet/loaddefs.el" cedet-root-path))
    (message "[tahti-cedet] Compiling cedet - please wait....")
    (shell-command (concat "make -C " cedet-root-path)))
  (unless (featurep 'cedet-devel-load)
    (message "[tahti-cedet] Loading Cedet..")
    (load (expand-file-name "cedet-devel-load.el" cedet-root-path)))
  (add-to-list 'load-path (expand-file-name "contrib" cedet-root-path))
  (add-to-list 'Info-directory-list (expand-file-name "doc/info" cedet-root-path))
  (add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)      ;enable semanticsdb
  (add-to-list 'semantic-default-submodes 'global-semantic-mru-bookmark-mode) ;enable jumping back
  (add-to-list 'semantic-default-submodes 'global-semantic-highlight-func-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode) ;parse when idle
  (add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)   ;show tag at the top
  (add-to-list 'semantic-default-submodes 'global-cedet-m3-minor-mode) ;activate context menu to rmb
  (add-to-list 'semantic-default-submodes 'global-semantic-decoration-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-idle-local-symbol-highlight-mode) ; highlighting of local names same as tag under cursor;
  (semantic-mode 1)  ;; activate semantic

  (require 'semantic/bovine/c)
  (require 'semantic/bovine/clang)
  (require 'semantic/bovine/gcc)
  (require 'semantic/ia)
  (require 'semantic/decorate/include)
  ; (require 'semantic/lex-spp)
  (require 'ede/srecode)
  ; loading contrib...
  (require 'eassist)
  ;(semantic-load-enable-minimum-features)
  ;(semantic-load-enable-code-helpers)

  ;;; idle
  (setq semantic-idle-scheduler-idle-time 1)
  (setq semantic-idle-work-parse-neighboring-files-flag t)

  ;;; decoration
  (global-semantic-decoration-mode 1)
  (global-semantic-idle-local-symbol-highlight-mode 1)
  (semantic-toggle-decoration-style "semantic-decoration-on-private-members" t)
  (semantic-toggle-decoration-style "semantic-decoration-on-protected-members" t)

  (setq semantic-idle-breadcrumbs-format-tag-list-function #'semantic-idle-breadcrumbs--format-innermost-first)
  (global-semantic-idle-breadcrumbs-mode 1)
  ;; Enable EDE (Project Management) features
  ;(global-ede-mode 1)
  ;(ede-enable-generic-projects)

  ;; SRecode for templates
  (global-srecode-minor-mode 1)

  (cogre-uml-enable-unicode)

  ;;; tags
  (semanticdb-enable-gnu-global-databases 'c-mode)
  (semanticdb-enable-gnu-global-databases 'c++-mode)
  (semanticdb-enable-gnu-global-databases 'java-mod)

  ;(semantic-load-enable-primary-ectags-support)
  ;(semantic-load-enable-primary-exuberent-ctags-support)




  ;; Support for java
  (require 'cedet-java);
  (require 'semantic/db-javap)
  ;###################################

  ;(defun my-cedet-hook ()
    ;(local-set-key [(control return)] 'semantic-ia-complete-symbol)
    ;(local-set-key "\C-c?" 'semantic-ia-complete-symbol-menu)
    ;(local-set-key "\C-c>" 'semantic-complete-analyze-inline)
    ;(local-set-key "\C-cp" 'semantic-analyze-proto-impl-toggle))
  ;(add-hook 'c-mode-common-hook 'my-cedet-hook);

  ;(add-hook 'c-mode-common-hook (gen-local-fill-keymap-hook
                                 ;"C-c c ?" 'semantic-documentation-for-tag
                                 ;"C-c c t" 'semantic-analyze-proto-impl-toggle
                                 ;"C-c c s" 'semantic-ia-show-summary
                                 ;"C-c c c" 'semantic-ia-describe-class))
  (tahti-cedet-keys)
)
(provide 'tahti-cedet)
