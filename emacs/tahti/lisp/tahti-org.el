(push 'org-mode el-get-packages)
(setq org-src-fontify-natively t) ;syntax highlight in code blocks in org mode
(setq org-confirm-babel-evaluate nil) ; do not ask to evaluate
(defun tahti-after-org-mode ()
  (org-babel-do-load-languages 
   'org-babel-load-languages '((shell . t)
                               (ditaa . t)
                               (python . t)))
(provide 'tahti-org)

