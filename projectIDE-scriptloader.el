;;; projectIDE-scriptloader.el --- projectIDE scriptloader
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
;; This file is part of projectIDE.
;; This files provides script laoding funtions.
;;
;;
;;; code:
(require 'projectIDE-header)
(require 'projectIDE-debug)

(defconst PROJECTIDE-SHELL-NAME "*projectIDE script loader*")



;;;###autoload
(defun projectIDE-maintain-shell-window ()

  "Keep the shell buffer selected and keep message displaying."

  (if (get-buffer-window PROJECTIDE-SHELL-NAME)
      (unless (eq (current-buffer) (get-buffer-window PROJECTIDE-SHELL-NAME))
        (select-window (get-buffer-window PROJECTIDE-SHELL-NAME)))

    (unless (eq (current-buffer) (get-buffer PROJECTIDE-SHELL-NAME))
      (projectIDE-other-window)
      (if (get-buffer PROJECTIDE-SHELL-NAME)
          (switch-to-buffer PROJECTIDE-SHELL-NAME)
        (switch-to-buffer (generate-new-buffer PROJECTIDE-SHELL-NAME))
        (setq-local buffer-read-only t))))

  (when (eq (current-buffer) (get-buffer PROJECTIDE-SHELL-NAME))
    (goto-char (point-max))
    (read-event nil nil 0.1)
    (recenter -3)
    (redisplay)))



;;;###autoload
(defun projectIDE-run-bash-file (file)

    "Run shell script (linux) with file path FILE.

FILE
Type:\t\t string
Descrip.: File path to the shell script file."

  (with-temp-message
      (projectIDE-message 'Info
                          (format "Running script: %s" file)
                          nil)

    (projectIDE-maintain-shell-window)
    
    (let ((process
           (start-process-shell-command
            (concat "*" (file-name-nondirectory file) "*")
            (get-buffer PROJECTIDE-SHELL-NAME)
            (concat "bash " file))))
      (while (process-live-p process)
        (projectIDE-maintain-shell-window)
        (sit-for 0.5)))))



;;;###autoload
(defun projectIDE-run-python-file (file)

    "Run python file with file path FILE.

FILE
Type:\t\t string
Descrip.: File path to the python file."
    
  (with-temp-message
      (projectIDE-message 'Info
                          (format "Running script: %s" file)
                          nil)

    (projectIDE-maintain-shell-window)
    
    (let ((process
           (start-process-shell-command
            (concat "*" (file-name-nondirectory file) "*")
            (get-buffer PROJECTIDE-SHELL-NAME)
            (concat "python " file))))
      (while (process-live-p process)
        (projectIDE-maintain-shell-window)
        (sit-for 0.5)))))



;;;###autoload
(defun projectIDE-run-bat-file (file)

  "Run batch file (windows) with file path FILE.

FILE
Type:\t\t string
Descrip.: File path to the bat file."

  (with-temp-message
      (projectIDE-message 'Info
                          (format "Running script: %s" file)
                          nil)

    (projectIDE-maintain-shell-window)
    
    (let ((process
           (start-process-shell-command
            (concat "*" (file-name-nondirectory file) "*")
            (get-buffer PROJECTIDE-SHELL-NAME)
            (concat "call " file))))
      (while (process-live-p process)
        (projectIDE-maintain-shell-window)
        (sit-for 0.5)))))



;;;###autoload
(defun projectIDE-run-shell-command (command)

  "Run COMMAND.

COMMAND
Type:\t\t string
Descrip.: Shell command."

  (with-temp-message
      (projectIDE-message 'Info
                          "Running shell command ... "
                          nil)

    (projectIDE-maintain-shell-window)
    
    (let ((process
           (start-process-shell-command
            (concat "*projectIDE shell command*")
            (get-buffer PROJECTIDE-SHELL-NAME)
            command)))
      (while (process-live-p process)
        (projectIDE-maintain-shell-window)
        (sit-for 0.5)))))


;;;###autoload
(defun projectIDE-shellify-path (elt)

  "Convert Emacs file path to shell file path if ELT is a file path.

ELT
Type:\t\t string
Descrip.:\t Convert Emacs file path to shell file path."
  
    (replace-regexp-in-string " " "\\ " elt nil t))


;;;###autoload
(defun projectIDE-load-script (first &rest arg)

  "A function loading and run scripts.
The script can be a single line of shell script or batch script.
Or the file path of several types of script file.
Currently, shell script, batch script and python script
files are supported."

  (with-current-buffer (get-buffer-create PROJECTIDE-SHELL-NAME)
    (setq default-directory (projectIDE-get-project-path (projectIDE-get-Btrace-signature)))
    (unless (eq major-mode 'compilation-mode)
      (compilation-mode)))
  
  (cond
   ((or (string= (file-name-extension first) "sh")
        (string= (file-name-extension first) "bash"))
    (if (file-executable-p first)
        (projectIDE-run-bash-file first)
      (when (file-executable-p (concat (projectIDE-get-project-path projectIDE-active-project) first))
        (projectIDE-run-bash-file (concat (projectIDE-get-project-path projectIDE-active-project) first)))))
   ((string= (file-name-extension first) "py")
    (if (file-executable-p first)
        (projectIDE-run-python-file first)
      (when (file-executable-p (concat (projectIDE-get-project-path projectIDE-active-project) first))
        (projectIDE-run-python-file (concat (projectIDE-get-project-path projectIDE-active-project) first)))))
   ((or (string= (file-name-extension first) "bat")
        (string= (file-name-extension first) "cmd"))
    (if (file-executable-p first)
        (projectIDE-run-bat-file first)
      (when (file-executable-p (concat (projectIDE-get-project-path projectIDE-active-project) first))
        (projectIDE-run-bat-file (concat (projectIDE-get-project-path projectIDE-active-project) first)))))
   (t
    (projectIDE-run-shell-command (mapconcat 'projectIDE-shellify-path (append (list first) arg) " "))
    (projectIDE-message 'Info
                        (format "Finsihed command: %s" (mapconcat 'identity (append (list first) arg) " "))
                        t))))


(provide 'projectIDE-scriptloader)
;;; projectIDE-scriptloader.el ends here
