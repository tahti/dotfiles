(require 'tahti-dirs)
(require 'tahti-util)

(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(defun display-prefix (arg)
       "Display the value of the raw prefix arg."
       (interactive "P")
       (message "%s" arg))


;; ---------------- Aliases --------------------------------------------
(unless (file-exists-p tahti-alias-file)
  (touch tahti-alias-file))

(defconst tahti-aliases
  (with-temp-buffer
    (insert-file-contents tahti-alias-file)
    (mapcar (lambda (line)
              (let ((split (split-string line " ")))
                (cons (cadr split) (car split))))
            (split-string (buffer-string) "\n" t))))

(defun tahti-find-helm-at-alias (alias)
  "Find file in aliased directory."
  (interactive (list (helm-completing-read-default "Alias: "
                                                  (mapcar #'car
                                                          tahti-aliases) nil t)))
  (let ((default-directory (cdr (assoc alias tahti-aliases))))
    (helm-find-files nil)))

(defun tahti-indent-buffer ()
  "Indents the entire buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun tahti-untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun tahti-cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (tahti-indent-buffer)
  (tahti-untabify-buffer)
  (whitespace-cleanup))

(defun save-buffer-always ()
  "Save the buffer even if it is not modified."
  (interactive)
  (set-buffer-modified-p t)
  (save-buffer))

(provide 'tahti-func)
