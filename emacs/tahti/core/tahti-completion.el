;use ac-slime ? hippie? auto-complete?
;; abbrev config ========================================
(add-hook 'text-mode-hook 'abbrev-mode)

;; yasnippet =============================================
(add-to-list 'yas-snippet-dirs tahti-snippets-dir)
(setq yas-prompt-functions '(yas-ido-prompt
                             yas-completing-prompt))
(setq yas-indent-line 'fixed)
(setq yas-triggers-in-field t)
(setq yas-verbosity 2)
(require-and-exec 'yasnippet
  (yas-global-mode 1))

(defun yas-org-very-safe-expand ()
  (let ((yas-fallback-behavior 'return-nil)) (yas-expand)))
(add-hook 'org-mode-hook
          (lambda ()
            ;; yasnippet (using the new org-cycle hooks)
            (add-to-list 'org-tab-first-hook 'yas-org-very-safe-expand)
            (define-key yas-keymap [tab] 'yas-next-field)))

(defun tahti/yas-expand-or-spc (count)
  (interactive "p")
  (let ((yas-fallback-behavior 'return-nil))
    (unless (yas-expand)
      (insert (make-string count ? )))))

(defun tahti/no-yas ()
  (setq yas--dont-activate t))

(add-to-hooks 'tahti/no-yas '(magit-mode-hook
                            calc-mode-hook))

(defun tahti/region-to-snippet (begin end)
  "Write new snippet based on current region."
  (interactive "r")
  (let ((region (buffer-substring begin end)))
    (yas-new-snippet)
    (save-excursion
      (goto-char (point-max))
      (insert region))))

(provide 'tahti-completion)

