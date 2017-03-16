(push 'org-mode el-get-packages)
(setq org-src-fontify-natively t) ;syntax highlight in code blocks in org mode
(setq org-confirm-babel-evaluate nil) ; do not ask to evaluate
(setq org-latex-create-formula-image-program 'dvipng)
(defun tahti-after-org-mode ()
  (require 'org)
  (require 'ox-latex)
  (org-babel-do-load-languages 
   'org-babel-load-languages '((shell . t)
                               (ditaa . t)
                               (latex . t)
                               (python . t)))
)
(provide 'tahti-org)

