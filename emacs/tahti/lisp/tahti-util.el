;;; tahit-util.el --- Based on cofi-util.el
(push 'queue el-get-packages)
(require 'cl)
;(require 'queue)

(defvar tahti/full-emacs t "Load all settings not just minimal.")
(defvar tahti/mail-instance nil "This is an email instance.")

(defmacro on-full-instance (&rest body)
  `(when tahti/full-emacs
     ,@body))


(defun add-to-hooks (fun hooks)
  "Add function to hooks"
  (dolist (hook hooks)
    (add-hook hook fun)))

(defun tahti/set-key (map spec cmd)
  "Set in `map' `spec' to `cmd'.

`Map' may be `'global' `'local' or a keymap.
A `spec' can be a `read-kbd-macro'-readable string or a vector."
  (let ((setter-fun (case map
                      (global #'global-set-key)
                      (local  #'local-set-key)
                      (t      (lambda (key def) (define-key map key def)))))
        (key (typecase spec
               (vector spec)
               (string (read-kbd-macro spec))
               (t (error "wrong argument")))))
    (funcall setter-fun key cmd)))
(defmacro require-and-exec (feature &rest body)
   "Require the feature and execute body if it was successfully loaded."
     (declare (indent defun))
       `(if (require ,feature nil 'noerror)
                (progn ,@body)
                (message (format "%s not loaded" ,feature))))

(defun fill-keymap (keymap &rest mappings)
  "Fill `KEYMAP' with `MAPPINGS'.
See `pour-mappings-to'."
  (pour-mappings-to keymap mappings))

(defmacro gen-local-fill-keymap-hook (&rest mappings)
  "Build fun that fills local keymap with `MAPPINGS'.
See `pour-mappings-to'."
  `(lambda () (fill-keymap 'local ,@mappings)))


(defun pour-mappings-to (map mappings)
  "Calls `tahti/set-key' with `map' on every key-fun pair in `MAPPINGS'.
`MAPPINGS' is a list of string-fun pairs, with a `READ-KBD-MACRO'-readable string and a interactive-fun."
  (dolist (mapping (group mappings 2))
    (tahti/set-key map (car mapping) (cadr mapping)))
  map)

(defun group (lst n)
  "Group `LST' into portions of `N'."
  (let (groups)
    (while lst
      (push (take n lst) groups)
      (setq lst (nthcdr n lst)))
    (nreverse groups)))

(defun take (n lst)
  "Return atmost the first `N' items of `LST'."
  (let (acc '())
    (while (and lst (> n 0))
      (decf n)
      (push (car lst) acc)
      (setq  lst (cdr lst)))
    (nreverse acc)))

(defmacro cmd (name &rest code)
  "Macro for shorter keybindings."
  `(defun ,(intern (concat "tahti-cmd/" (symbol-name name))) ()
     (interactive)
     ,@code))

(defun range (end-or-start &optional end step)
  "Range of numbers from `START' to (including) `END' with stepwidth `STEP'.
If only one argument is supplied it will be the end, 0 will be start.
Mimicks Python's `range'"
  (let ((step (or step 1))
        (start (if end
                   end-or-start
                 0))
        (end (if end
                 end
               end-or-start)))
    (loop for i from start to end by step
          collect i)))

(defun dot? (fname &optional dotfiles)
  "Determines if `FNAME' is a dot or dotfile if `DOTFILES' is non-nil."
  (let ((f (file-name-nondirectory fname)))
     (if dotfiles
         (string-prefix-p "." f)
       (or (string= "." f) (string= ".." f)))))

(defun ls-no-dots (directory &optional full dotfiles match)
  "Returns files in `directory' without `.' and `..'.
`full', `match' and `nosort' act as in `directory-files'"
    (remove-if (lambda (f) (dot? f dotfiles))
               (directory-files directory full match)))

(defun enqueue-all (queue l)
  "Enqueues in `QUEUE' all entries of `L'."
  (mapc (lambda (e) (queue-enqueue queue e))
        l))

(defun ls-dirs (directory &optional dotfiles match)
  "Returns all dirs in `DIR'.
`DOTFILES' -- if non-nil don't include dirs starting with a `.'
`MATCH' -- if non-nil only include dirs matching the regexp"
  (remove-if-not #'file-directory-p
                 (ls-no-dots directory t dotfiles match)))

(defun ls-files (directory &optional dotfiles match)
  "Returns all files in `DIR'.
`DOTFILES' -- if non-nil don't include files starting with a `.'
`MATCH' -- if non-nil only include files matching the regexp"
  (remove-if #'file-directory-p
             (ls-no-dots directory t dotfiles match)))

(defun ls-files-deep (dir &optional dotfiles fmatch dmatch)
  "Returns all files within `DIR'.
`DOTFILES' -- if non-nil don't include files and dirs starting with a `.'
`FMATCH' -- if non-nil only include files matching the regexp
`DMATCH' -- if non-nil only include files in dirs matching the regexp; if parent
            dir failed its dirs will not be searched."
  (let ((dirs (queue-create)))
    (queue-enqueue dirs dir)
    (loop while (> (queue-length dirs) 0)
          nconc (let ((d (queue-dequeue dirs)))
                  (enqueue-all dirs (ls-dirs d dotfiles dmatch))
                  (ls-files d dotfiles fmatch)))))

(defun ls-files-deep-1 (dir &optional dotfiles fmatch dmatch)
  "Returns all files within `DIR' descending one level.
`DOTFILES' -- if non-nil don't include files and dirs starting with a `.'
`FMATCH' -- if non-nil only include files matching the regexp
`DMATCH' -- if non-nil only include and search dirs matching the regexp"
  (let ((dirs (cons dir (ls-dirs dir dotfiles dmatch))))
    (loop for d in dirs
          nconc (ls-files d dotfiles fmatch))))

(defun touch (filename)
     "updates mtime on the file for the current buffer"
     (interactive)
     (shell-command (concat "touch " (shell-quote-argument filename)))
     (clear-visited-file-modtime))

;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

(defun copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(defun copy-short-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (file-name-nondirectory buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(defun underline-with-char (char)
  (interactive (list (read-from-minibuffer "Char: ")))
  (when (= 0 (length char))
    (error "Need a character"))
  (setq char (aref char 0))             ; Ignore everything but the first char.
  (save-excursion
    (goto-char (point-at-eol))
    (insert "\n" (make-string (- (point-at-eol)
                            (point-at-bol))
                         char))))
(defun underline-with-char-equal ()
  (interactive)
   (underline-with-char "="))

(defun underline-with-char-minus ()
  (interactive)
   (underline-with-char "-"))

(defun underline-with-char-tilde ()
  (interactive)
   (underline-with-char "~"))

(defun underline-with-char-up ()
  (interactive)
   (underline-with-char "^"))

(defun underline-with-char-plus ()
  (interactive)
   (underline-with-char "+"))


(provide 'tahti-util)

