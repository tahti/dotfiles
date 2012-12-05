(require 'tahti-dirs)
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
    (:name predictive 
           :type http-tar
           :url "http://www.dr-qubit.org/download.php?file=predictive/predictive-0.23.13.tar.gz"
           :options ("xzf")
           ) ;uncomment if original recipe is too slow
    (:name cedet  ;original url does not work behind firewall
           :type git
           :url "https://github.com/emacsmirror/cedet.git"
    ) 
    (:name vimvars
           :description "Provide support for VI-style mode lines in Emacs."
           :type git
           :url "https://github.com/emacsmirror/vimvars.git"
           :features "vimvars"
    )
    (:name queue
           :description "A queue can be used as both a first-in last-out and a first-in first-out stack, i.e. elements can be added to and removed from either end of the queue."
           :type http
           :url "http://www.dr-qubit.org/predictive/queue.el"
           :features "queue"
    )
    (:name powerline  ;use extended version of powerline
           :description "Emacs version of the Vim powerline."
           :type git
           :url "https://github.com/milkypostman/powerline.git"
           :features "powerline"
    )    ;;uncomment if you want to add as submodule in vendor directory
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

