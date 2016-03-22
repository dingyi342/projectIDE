(require 'projectIDE-header)
(defcustom projectIDE-enable-session t)

(cl-defstruct projectIDE-session
  point
  word
  time)

(defun projectIDE-save-session ()
  (when projectIDE-enable-session
   (let ((signature (projectIDE-get-Btrace-signature))
          (record (and signature
                       (projectIDE-get-module-persist-memory signature 'projectIDE-session)
                       (make-hash-table :test 'equal))))
      (puthash
       (buffer-file-name)
       (make-projectIDE-session :point (point)
                                :point (word-at-point)
                                :time (current-time))
       session)
      
      (projectIDE-set-module-persist-memory signature 'projectIDE-session session))))

(defun projectIDE-restore-session ()
  (when projectIDE-enable-session
    (let ((signature (projectIDE-get-Btrace-signature))
          (record (and signature
                       (projectIDE-get-module-persist-memory signature 'projectIDE-session)))
          (session (and record (gethash (buffer-file-name) record))))

      (when (projectIDE-session-p session)
        (when (<= (projectIDE-session-point session) (point-max))
          (goto-char (projectIDE-session-point session)))
        (unless (equal (word-at-point) (projectIDE-session-point))
          (goto-char (point-min)))))))

(add-hooks)

(provide 'projectIDE-session)
