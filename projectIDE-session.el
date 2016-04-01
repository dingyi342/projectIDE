;;; projectIDE-session.el --- projectIDE session file
;;
;; Copyright (C) 2015-2016 Mola-T
;; Author: Mola-T <Mola@molamola.xyz>
;; URL: https://github.com/mola-T/projectIDE
;; Version: 1.0
;; Keywords: project, convenience
;;
;;; License:
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;;
;;; Commentary:
;;
;; This file is a part of projectIDE.
;; It helps saving and restoring project buffer point position
;;
;;; code:
(require 'cl-lib)
(require 'projectIDE-header)
(require 'projectIDE-debug)

(defgroup projectIDE-session nil
  "Setting for projectIDE session."
  :tag "Session"
  :group 'projectIDE)

(defcustom projectIDE-enable-session t
  "Remember point location when closing buffer of a point
so that it can be restored."
  :tag "Enable session?"
  :type 'bool
  :group 'projectIDE-session)

(defcustom projectIDE-session-limit 80
  "The maximum number of session saved for each project."
  :tag "Session limit"
  :type 'integer
  :group 'projectIDE-session)

(defcustom projectIDE-session-reduce-number 40
  "The number of session will be reduce when session limit is reached."
  :tag "Session reduce number"
  :type 'integer
  :group 'projectIDE-session)

(defvar projectIDE-session-timer nil
  "Store the timer of `projectIDE-session-timer-function'.
Never attempt to modify it directly.")

(cl-defstruct projectIDE-session
  point
  word
  time)



(defun projectIDE-save-session ()
  
  "Save the location of point when project buffer is closed."
  (when (and projectIDE-enable-session
             (not (string= (car-safe (projectIDE-get-module-var (projectIDE-get-Btrace-signature) 'projectIDE-session)) "f")))
    (let* ((signature (projectIDE-get-Btrace-signature))
           (session-hash (and signature
                              (projectIDE-get-module-persist-memory signature 'projectIDE-session))))
      
      (unless session-hash
        (setq session-hash (make-hash-table :test 'equal)))
      
      (when (>= (hash-table-count session-hash) projectIDE-session-limit)
        (projectIDE-message 'Info
                            "Cleaning session cache ..."
                            t)
        (let ((files (hash-table-keys session-hash))
              remove-files)

          (dolist (file files)
            (unless (file-exists-p file)
              (remhash file session-hash)))

          (setq files (hash-table-keys session-hash))
          (sort files (lambda (file1 file2)
                        (time-less-p
                         (projectIDE-session-time (gethash file2 session-hash))
                         (projectIDE-session-time (gethash file1 session-hash)))))

          (setq remove-files (cl-subseq files (- projectIDE-session-limit projectIDE-session-reduce-number)))
          (dolist (file remove-files)
            (remhash file session-hash))))
      
      (puthash
       (buffer-file-name)
       (make-projectIDE-session :point (point)
                                :word (word-at-point)
                                :time (current-time))
       session-hash)
      
      (projectIDE-set-module-persist-memory signature 'projectIDE-session session-hash))))



(defun projectIDE-restore-session ()

  "Restore the location of point when project buffer is opened."
  
    (when (and projectIDE-enable-session
             (not (string= (car-safe (projectIDE-get-module-var (projectIDE-get-Btrace-signature) 'projectIDE-session)) "f")))
    (let* ((signature (projectIDE-get-Btrace-signature))
           (session-hash (and signature
                        (projectIDE-get-module-persist-memory signature 'projectIDE-session)))
           (session (and session-hash (gethash (buffer-file-name) session-hash))))
      
      (when (projectIDE-session-p session)
        (when (<= (projectIDE-session-point session) (point-max))
          (goto-char (projectIDE-session-point session)))
        (if (equal (word-at-point) (projectIDE-session-word session))
            (progn
              (message (buffer-file-name))
              (recenter))
          (goto-char (point-min)))))))



(defun projectIDE-keep-opened-buffer-order ()
  
  "Advice after `set-buffer' to maintain opened buffer order.
But this is not add to advice directly.  Instead, it is encapsulated
by a timer function and ther timer function is the function that
actually advice after `set-buffer' so that `projectIDE-get-Btrace-signature'
can get a correct signature."
  
  (when (and (projectIDE-get-Btrace-signature) projectIDE-enable-session)
    (projectIDE-add-opened-buffer (projectIDE-get-Btrace-signature) (buffer-file-name)))
  (setq projectIDE-session-timer nil))



(defun projectIDE-session-timer-function (&rest args)

  "Encapsulate `projectIDE-keep-opened-buffer-order' to a timer.
Advice this function after `set-buffer'."
  
  (unless projectIDE-session-timer
    (setq projectIDE-session-timer
          (run-with-idle-timer 0 nil 'projectIDE-keep-opened-buffer-order))))



(defun projectIDE-session-initialize ()

  "Initialize projectIDE session."
  
  (add-hook 'projectIDE-open-project-buffer-hook 'projectIDE-restore-session)
  (add-hook 'projectIDE-kill-project-buffer-hook 'projectIDE-save-session)
  (advice-add 'set-buffer :after 'projectIDE-session-timer-function))



(defun projectIDE-session-terminate ()

  "Terminate projectIDE-session."

  (remove-hook 'projectIDE-open-project-buffer-hook 'projectIDE-restore-session)
  (remove-hook 'projectIDE-kill-project-buffer-hook 'projectIDE-save-session)
  (advice-remove 'set-buffer 'projectIDE-session-timer-function))

(provide 'projectIDE-session)
;;; projectIDE-session.el ends here
