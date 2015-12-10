(require 'bindat)
(require 'json)

(defconst i3-process-name "*i3-process*"
  "Name of the buffer associated with i3 IPC process")

(defun tahti-i3-command (command &optional payload)
  "Sends command to i3 and returns the response. See i3 wm IPC docs for details."
  (let ((proc (tahti-i3-get-or-make-client))
        (length (length payload)))
    (process-put proc 'response nil)
    (process-put proc 'partial-response nil)
    (process-send-string proc (concat (bindat-pack i3-msg-spec `((:magic . "i3-ipc")
                                                                 (:length . ,length)
                                                                 (:command . ,command)))
                                      payload))
    (while (not (process-get proc 'response))
      (accept-process-output proc))
    (let ((response (process-get proc 'response)))
      (process-put proc 'response nil)
      (json-read-from-string response))))

(defun tahti-i3-get-or-make-client ()
  "Creates or returns a current process connected to i3 wm IPC."
  (unless (boundp 'i3-client)
    (setq i3-client (make-network-process :name i3-process-name :buffer (get-buffer-create i3-process-name)
                                          :coding '(raw-text-unix . raw-text-unix)
                                          :family 'local
                                          :filter 'i3-response-filter
                                          :sentinel 'i3-sentinel
                                          :service (i3-chomp (shell-command-to-string "i3 --get-socketpath")))))
  i3-client)

;;; Internal functions and data structures.

(defvar i3-msg-spec
  '((:magic str 6)
    (:length u32r)
    (:command u32r)))

(defun i3-response-filter (proc resp)
  (if (not (process-get proc 'partial-response))
      (let ((length (cdr (assq :length (bindat-unpack i3-msg-spec resp))))
            (payload (substring resp 14)))
        (process-put proc 'partial-response-length length)
        (process-put proc 'partial-response payload))
    (process-put proc 'partial-response (concat (process-get proc 'partial-response) resp)))
  (when (>= (length (process-get proc 'partial-response)) (process-get proc 'partial-response-length))
    (process-put proc 'response (process-get proc 'partial-response))
    (process-put proc 'partial-response nil)))

(defun i3-sentinel (proc event)
  (setq i3-client nil))

(defun i3-chomp (string)
  (replace-regexp-in-string "\n$" "" string))

(provide 'tahti-i3)
