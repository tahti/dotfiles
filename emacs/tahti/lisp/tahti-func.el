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

(defun tahti-cd-alias (alias)
  "Change directory to aliased one."
  (interactive (list (ido-completing-read "Alias: "
                                          (mapcar #'car
                                                  tahti-aliases) nil t)))
  (let ((dir (cdr (assoc alias tahti-aliases))))
    (cd dir)))

(defun tahti-dired-alias (alias)
  "Open dired on aliased directory."
  (interactive (list (ido-completing-read "Alias: "
                                          (mapcar #'car
                                                  tahti-aliases) nil t)))
  (let ((dir (cdr (assoc alias tahti-aliases))))
    (dired dir)))

(defun tahti-find-at-alias (alias)
  "Find file in aliased directory."
  (interactive (list (ido-completing-read "Alias: "
                                          (mapcar #'car
                                                  tahti-aliases) nil t)))
  (let ((dir (cdr (assoc alias tahti-aliases))))
    (ido-find-file-in-dir dir)))

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

;; Taken from http://www.emacswiki.org/emacs/ArtistMode
(defun artist-ido-select-operation (type)
  "Use ido to select a drawing operation in artist-mode"
  (interactive
   (list (ido-completing-read "Drawing operation: " 
           (list "Pen" "Pen Line" "line" "straight line" "rectangle" 
                 "square" "poly-line" "straight poly-line" "ellipse" 
                 "circle" "text see-thru" "text-overwrite" "spray-can" 
                 "erase char" "erase rectangle" "vaporize line" "vaporize lines" 
                 "cut rectangle" "cut square" "copy rectangle" "copy square" 
                 "paste" "flood-fill"))))
  (artist-select-operation type))

(defun artist-ido-select-settings (type)
  "Use ido to select a setting to change in artist-mode"
  (interactive
   (list (ido-completing-read "Setting: " 
           (list "Set Fill" "Set Line" "Set Erase" "Spray-size" "Spray-chars" 
                 "Rubber-banding" "Trimming" "Borders"))))
  (if (equal type "Spray-size") 
      (artist-select-operation "spray set size")
    (call-interactively (artist-fc-get-fn-from-symbol 
                         (cdr (assoc type '(("Set Fill" . set-fill)
                                            ("Set Line" . set-line)
                                            ("Set Erase" . set-erase)
                                            ("Rubber-banding" . rubber-band)
                                            ("Trimming" . trimming)
                                            ("Borders" . borders)
                                            ("Spray-chars" . spray-chars))))))))
(defun save-buffer-always ()
  "Save the buffer even if it is not modified."
  (interactive)
  (set-buffer-modified-p t)
  (save-buffer))

(provide 'tahti-func)
