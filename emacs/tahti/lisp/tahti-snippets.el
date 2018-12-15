(require 'tahti-dirs)
(require 'tahti-keys)

(push 'yasnippet el-get-packages)
(defun tahti-after-yasnippet ()
  ;(setq yas-snippet-dirs '("~/.emacs.d/el-get/yasnippet/snippets"))
  ;(add-to-list 'yas-snippet-dirs tahti-snippets-dir)
;; Disable the load of the default snippets, I prefer to use mine or none of
;; them
;;
;; Even without snippets this is useful for package like irony-mode that uses
;; yasnippet as a dynamic snippet library.
  (setq yas-snippet-dirs (list tahti-snippets-dir)) 
  (yas-global-mode 1)
  (tahti-yas-keys )
)


;(defun yas-org-very-safe-expand ()
  ;(let ((yas-fallback-behavior 'return-nil)) (yas-expand)))
;(add-hook 'org-mode-hook
          ;(lambda ()
            ;;; yasnippet (using the new org-cycle hooks)
            ;(add-to-list 'org-tab-first-hook 'yas-org-very-safe-expand)
            ;(define-key yas-keymap [tab] 'yas-next-field)))

;(defun tahti/yas-expand-or-spc (count)
  ;(interactive "p")
  ;(let ((yas-fallback-behavior 'return-nil))
    ;(unless (yas-expand)
      ;(insert (make-string count ? )))))

(defun tahti-no-yas ()
  (setq yas-dont-activate t))

(add-to-hooks 'tahti-no-yas '(magit-mode-hook
                            calc-mode-hook
                            term-mode-hook))

;(defun tahti/region-to-snippet (begin end)
  ;"Write new snippet based on current region."
  ;(interactive "r")
  ;(let ((region (buffer-substring begin end)))
    ;(yas-new-snippet)
    ;(save-excursion
      ;(goto-char (point-max))
      ;(insert region))))

(provide 'tahti-snippets)

