
(defun tahti-filter-el-get-sources (recipe)
  (let* ((name (symbol-name (plist-get recipe :name)))
         ;(func (intern (concat "tahti-el-get-after-" name)))
         (url (plist-get recipe :url)))

    ;; use iy-el-get-after-* as after function if defined
    ;(when (fboundp func)
      ;(plist-put recipe :after (list func)))
    ;(when (fboundp func)
      ;(message (concat "Hooked function: tahti-el-get-after-" name)))
    (when (eq url 'vendor)
      (plist-put recipe :url (concat tahti-vendor-dir name)))
    recipe))
(setq
 el-get-sources
 (mapcar
  'tahti-filter-el-get-sources
  `(
    (:name helm-descbinds 
           :type git
           :url "https://github.com/emacs-helm/helm-descbinds.git"
           )
    (:name undo-tree :url "https://github.com/emacsmirror/undo-tree.git") ;original url is too slow...
    (:name evil :type elpa) ;original stopped working on 27-11-2012
    (:name cedet  ;original url does not work behind firewall
           :type git
           :url "https://github.com/emacsmirror/cedet.git"
    ) 
    (:name vimvars
           :description "Provide support for VI-style mode lines in Emacs."
           :type git
           :url "https://github.com/emacsmirror/vimvars.git"
    )
    (:name menu-bar+
       :description "Separation of Local and Global Menus"
       :type emacswiki)
    ;;uncomment if you want to add as submodule in vendor directory
    (:name evil)
    ;(:name helm
           ;:website "https://github.com/emacs-helm/helm"
           ;:description "Emacs incremental completion and selection narrowing framework"
           ;:type git
           ;:url vendor
           ;:depends (org-mode emacs-w3m)
           ;:build `(,(concat "make LOADPATH='-L . -L ../org-mode/lisp' EMACS='" el-get-emacs " -Q -batch'"))
           ;:features helm-config
           ;:autoloads "helm-config")
    ;(:name org-mode :url vendor)     
    )))

(provide 'tahti-el-get)

