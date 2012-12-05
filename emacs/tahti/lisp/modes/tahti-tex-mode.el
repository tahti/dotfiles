(require 'tahti-keys)
(push 'auctex el-get-packages)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
;set AucTeX options
(custom-set-variables
  '(LaTeX-equation-label "eqn") ;prefix label for equations
  '(LaTeX-figure-label   "fig") ;prefix label for figures
  '(LaTeX-table-label    "tab") ;prefix label for tables
  '(LaTeX-float          "htbp") ;default float position
  '(LaTeX-default-position  "htbp") ;default tabular position
  '(LaTeX-math-abbrev-prefix "'")   ;prefix key for inserting math
  '(LaTeX-indent-level 2)
  '(TeX-newline-function  'reindent-then-newline-and-indent) ;indent on enter
)
;         '(TeX-font-list('(
;;          (?\C-b "{\\bf " "}")
;; 			   (?\C-c "{\\sc " "}")
;; ;; 			   (c "{\\sc " "}")
;; 			   (?\C-e "{\\em " "\\/}")
;; ;; 			   (e "{\\em " "\\/}")
;; 			   (?\C-i "{\\it " "\\/}")
;; ;; 			   (i "{\\it " "\\/}")
;; 			   (?\C-r "{\\rm " "}")
;; ;; 			   (r "{\\rm " "}")
;; 			   (?\C-s "{\\sl " "\\/}")
;; ;; 			   (s "{\\sl " "\\/}")
;; 			   (?\C-t "{\\tt " "}")
;; ;; 			   (t "{\\tt " "}")
;; 			   (?\C-d "" "" t)
;; ;; 			   (d "" "" t)
;;          ))))

(defun tahti-tex-mode-init ()
  ;; (turn-on-cdlatex)
  (turn-on-reftex)
  (turn-on-auto-fill)     ;auto break the lines to fit 80 columns
  (tahti-latex-keys)
  (LaTeX-math-mode)
  (TeX-fold-mode 1)
;;   (flyspell-mode 1)
;;   (TeX-PDF-mode 1)
)
(add-hook 'LaTeX-mode-hook 'tahti-tex-mode-init)
(add-hook 'ConTeXt-mode-hook 'tahti-tex-mode-init)
(provide 'tahti-tex-mode)

