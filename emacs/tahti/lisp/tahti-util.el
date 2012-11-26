;;; tahit-util.el --- Based on cofi-util.el
;(push 'queue el-get-packages)
(require 'cl)

(defvar tahti/full-emacs t "Load all settings not just minimal.")
(defvar tahti/mail-instance nil "This is an email instance.")

(defmacro on-full-instance (&rest body)
  `(when tahti/full-emacs
     ,@body))

(defun f-alt (&rest alternatives)
  "Test functions in `alternatives' and return first bound."
  (catch 'found
    (dolist (f alternatives)
      (if (functionp f)
          (throw 'found f)))))

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

(defmacro require-and-exec (feature &optional &rest body)
  "Require the feature and execute body if it was successfull loaded."
  (declare (indent 1))
  `(if (require ,feature nil 'noerror)
        (progn ,@body)
    (message (format "%s not loaded" ,feature))))

(defmacro load-and-exec (file &optional &rest body)
  "Load the file and execute body if it was successfull loaded."
  (declare (indent 1))
  `(if (load ,file t)
        (progn ,@body)
    (message (format "%s not loaded" ,file))))

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

(defun touch (filename)
     "updates mtime on the file for the current buffer"
     (interactive)
     (shell-command (concat "touch " (shell-quote-argument filename)))
     (clear-visited-file-modtime))

(provide 'tahti-util)

