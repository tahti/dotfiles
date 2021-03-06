(require 'tahti-keys)
(push 'auctex el-get-packages)
;(push 'reftex el-get-packages)
(push 'cdlatex-mode el-get-packages)

;(setq TeX-auto-save t) ;create auto directory for multiple documents
(setq TeX-parse-self t
      TeX-electric-escape nil ;do not call TeX-electric-macro when \ is typed
      ;reftex-plug-into-AUCTeX t
      TeX-insert-braces nil
      TeX-source-correlate-method 'synctex
      TeX-display-help t 
      LaTeX-version "2e")
(setq-default TeX-master nil)

;set AucTeX options
(custom-set-variables
  '(LaTeX-equation-label "eqn") ;prefix label for equations
  '(LaTeX-figure-label   "fig") ;prefix label for figures
  '(LaTeX-table-label    "tab") ;prefix label for tables
  '(LaTeX-float          "htbp") ;default float position
  '(LaTeX-default-position  "htbp") ;default tabular position
  ;'(LaTeX-math-abbrev-prefix "\"")   ;prefix key for inserting math
  '(LaTeX-indent-level 2)
  '(TeX-interactive-mode nil)
  '(TeX-newline-function  'newline-and-indent) ;indent on enter
  '(TeX-auto-untabify t)   ;remove tabs befor saving
  '(Tex-install-font-lock font-latex-setup) ;different syntax highlight
  '(font-latex-match-slide-title-keywords '(("frametitle" "{")))
  '(font-latex-match-warning-keywords '("hline" "pause" "and" "hfill"))
  '(font-latex-match-function-keywords
    '(("titlepage" "") ("maketitle" "") ("frame" "") ("tableofcontents" "")
     ("noindent" "") ("usetheme" "{") ("usecolortheme" "{") ("institute" "[{")
     ("includegraphics" "[{") ("title" "[{") ("href" "{{") ("url" "{") ("hypersetup" "{")
     ("useoutertheme" "{") ("useinnertheme" "{") ("setbeamercolor" "{{")
     ("setbeamertemplate" "{{") ("setdescription" "{") ("lstset" "{") ("lstinputlisting" "[{")
     ("textcolor" "{") ("verbatiminput" "{") ("graphicspath" "{")
     ("fancyhead" "[{") ("fancyfoot" "[{") ("fontsize" "{{") ("doublespacing" "")
     ("declinst" "[{{{") ("nextlevel" "[") ("NeedsTeXFormat" "{[")
     ("ProvidesPackage" "{[") ("RequirePackage" "{") ("\ProcessPgfPackageOptions" "{") ("usetikzlibrary" "{")
     ("usepgflibrary" "{") ("typeout" "{") ("PackageError" "{{{")  ("pgfkeysvalueof" "{")
     ("pgfkeysifdefined" "{") ("newif" "") ("csdef" "") ("newdimen" "") ("\PackageWarning" "{")
   ))
  '(font-latex-match-variable-keywords '(("newcount") ("def") ("edef") ("xdef") ("gdef")
     ("advance") ("if") ("else") ("fi") ("ifx")("ifnum") ("relax") ("foreach")
     ("pgfmathresult") ("pgfmathparse") ("mscset" "{")("mscget" "{") ("pgfkeys" "{")
     ("tikzset" "{") ("\pgfutil@ifundefined" "{") ("equal" "{{") ("or")
     ("draw" "[")
   ))
)
;; customize viewer
(setq TeX-view-program-list (quote (("okular" "okular -unique %u"))))

(defun okular-make-url () (concat
    "file://"
    (expand-file-name (funcall file (TeX-output-extension) t)
      (file-name-directory (TeX-master-file)))
    "#src:"
    (TeX-current-line)
    (TeX-current-file-name-master-relative)))
(setq TeX-view-program-selection '((output-pdf "Okular") (output-ps "Okular") (output-dvi "Okular")))

;; ===== Flymake for tex-mode ====

;; flymake-mode for tex uses texify by default, which only works in Windows (miktex)

;; Borrowed from https://github.com/MassimoLauria/dotemacs/blob/master/init-latex.el
;(defun init-latex--flymake-setup ()
  ;"Setup flymake for latex using one of the checker available on the system.
;It either tries \"lacheck\" or \"chktex\"."
  ;(interactive)
  ;(cond ((executable-find "lacheck")
         ;(defun flymake-get-tex-args (file-name)
           ;(list "lacheck" (list file-name))))
        ;((executable-find "chktex")
         ;(defun flymake-get-tex-args (file-name)
           ;(list "chktex" (list "-q" "-v0" file-name))))
        ;(t nil)))

;(eval-after-load "flymake" '(init-latex--flymake-setup))


;; Initialisation
(defun tahti-tex-mode-init ()
  (turn-on-cdlatex)
  ;(turn-on-reftex)
  (turn-on-auto-fill)     ;auto break the lines to fit 80 columns
  (LaTeX-math-mode 1)
  (TeX-fold-mode 1)
  (TeX-PDF-mode 1)
  (TeX-source-correlate-mode 1) ;activate synctex
  (TeX-source-specials-mode 1)
  (setq TeX-master (guess-TeX-master (buffer-file-name)))
  (add-to-list 'TeX-expand-list '("%u" okular-make-url))
  (ispell-change-dictionary "british")
  (tahti-latex-keys)
)
;; Some macros
    (defun guess-TeX-master (filename)
      "Guess the master file for FILENAME from currently open .tex files."
      (let ((candidate nil)
            (filename (file-name-nondirectory filename)))
        (save-excursion
          (dolist (buffer (buffer-list))
            (with-current-buffer buffer
              (let ((name (buffer-name))
                    (file buffer-file-name))
                (if (and file (string-match "\\.tex$" file))
                    (progn
                      (goto-char (point-min))
                      (if (re-search-forward (concat "\\\\input{" filename "}") nil t)
                          (setq candidate file))
                      (if (re-search-forward (concat "\\\\include{" (file-name-sans-extension filename) "}") nil t)
                          (setq candidate file))))))))
        (if candidate
            (message "TeX master document: %s" (file-name-nondirectory candidate)))
        candidate))

(add-hook 'LaTeX-mode-hook 'tahti-tex-mode-init)
(add-hook 'ConTeXt-mode-hook 'tahti-tex-mode-init)
(provide 'tahti-tex-mode)

