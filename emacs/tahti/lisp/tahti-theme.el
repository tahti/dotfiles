(require 'tahti-dirs)

(add-to-list 'custom-theme-load-path tahti-themes-dir)

(defvar tahti/current-colorscheme nil)
(defvar tahti/colorschemes '("cofi-dark" "cofi-light" "tahti-dark"))
(defun tahti/colorscheme (scheme)
  "Move to next colorscheme. If `ARG' is non-nil reload current."
  (interactive (list (completing-read "Colorscheme: " tahti/colorschemes)))
  (let ((custom-safe-themes t)
        (scheme (if (stringp scheme) (intern scheme) scheme)))
    (disable-theme tahti/current-colorscheme)
    (load-theme scheme)
    (setq tahti/current-colorscheme scheme)))

;;; additional faces
;(defface mode-line-buffer
  ;'((t (:bold t :foreground "#FFAA00")))
  ;""
  ;:group 'mode-line-faces)
;(defface mode-line-major-mode
  ;'((t (:bold t :foreground "gold")))
  ;""
  ;:group 'mode-line-faces)
;(defface mode-line-minor-mode
  ;'((t (:foreground "khaki")))
  ;""
  ;:group 'mode-line-faces)

(tahti/colorscheme 'tahti-dark)

(provide 'tahti-theme)
