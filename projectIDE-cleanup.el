;;; projectIDE-cleanup.el --- projectIDE cleanup -*- lexical-binding: t -*-
;;
;; Copyright (C) 2015-2016 Mola-T
;; Author: Mola-T <Mola@molamola.xyz>
;; URL: https://github.com/mola-T/projectIDE
;; Version: 1.0
;; Package-Requires: ((cl-lib.el "0.5"))
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
;; This files provides cleanup funtions.
;;
;; Interactive functions:
;; projectIDE-cleanup
;; projectIDE-restore-cleanup
;;
;; Config file:
;; projectIDE-cleanup
;;
;;; code:
(require 'projectIDE-header)

(defconst PROJECTIDE-CLEANUP-FOLDER ".projectIDE_cleanup"
  "The cleanup folder name.")



(defun projectIDE-cleanup-folder-predicate (elt)

  "Use to prevent \".projectIDE_cleanup\" being recursively cleanup.
Return t if ELT match \"projectIDE_cleanup\".

Return
Type:\t\t bool
Descrip.:\t Return t if ELT match \"projectIDE_cleanup\".

ELT
Type:\t\t string
Desctip.:\t Test path"

  (if (string-match "\\.projectIDE_cleanup" elt)
      t
    nil))



(defun projectIDE-shadow-copy (projectRoot from to)

  "Shodow copy folder/file within a project.

PROJECTROOT
Type:\t\t string
Descrip.:\t Path to project root.

FROM
Type:\t\t string
Descript.:\t The relative path of folder/file to PROJECTROOT
\t\t\t that needs to be copied.

TO
Type:\t\t string
Descript.:\t The destination folder of copying."

  (setq to (concat projectRoot to from)
        from (concat projectRoot from))

  (if (file-directory-p from)
      (mkdir to t)
    (unless (file-directory-p (file-name-directory to))
      (mkdir (file-name-directory to) t))
    (copy-file from to nil t t t)))



(defun projectIDE-delete-file (file)

  "Delete FILE.

FILE
Type:\t\t string
Descript.:\t File path."
  
  (when (file-exists-p file)
    (delete-file file)))

(defun projectIDE-delete-directory (folder)

    "Delete FOLDER.

FILE
Type:\t\t string
Descript.:\t File path."
    
  (when (file-directory-p folder)
    (delete-directory folder t)))


(defun projectIDE-restore-directory (from to)

  "Restore a shadow copied direcoty.

FROM
Type:\t\t string
Descrip.:\t The restoring source.

TO
Type:\t\t string
Descrip.:\t The restoring destination."
  
  (when (file-directory-p from)
    (let ((contents (directory-files from)))
      (dolist (content contents)
        (when (and (not (string= "." content)) (not (string= ".." content)))
          (if (file-directory-p (concat from content))
              (progn
                (unless (file-exists-p (concat to content))
                  (mkdir (concat to content)))
                (projectIDE-restore-directory (file-name-as-directory (concat from content))
                                              (file-name-as-directory (concat to content))))
            (unless (file-exists-p (concat to content))
              (copy-file (concat from content) (concat to content) nil t t t))))))))



(defun projectIDE-cleanup ()

  "An interactive function to cleanup unwanted folders/files.
Variable \"projectIDE-cleanup\" needs to be set at the config file.
For example, \"projectIDE-cleanup = *.elc\" will cleanup all elc files." 

  (interactive)

  (catch 'Error

    (unless (projectIDE-get-Btrace-signature)
      (projectIDE-message 'Warning
                          "Current buffer is not part of a project."
                          t
                          (projectIDE-caller 'projectIDE-compiler))
      (throw 'Error nil))

    (projectIDE-message 'Info
                        "Cleaning up, please wait..."
                        t
                        (projectIDE-caller 'projectIDE-cleanup))

    (when (projectIDE-important-cmd-update-cache? (projectIDE-get-Btrace-signature))
      (projectIDE-update-cache))

    (let ((signature (projectIDE-get-Btrace-signature))
          (string-list (projectIDE-get-module-var (projectIDE-get-Btrace-signature) 'projectIDE-cleanup))
          (cleanup-dir (file-name-as-directory
                        (concat (file-name-as-directory PROJECTIDE-CLEANUP-FOLDER) (format-time-string "%Y%m%d_%H-%M-%S"))))
          projectRoot
          remove-folderlist
          remove-filelist)

      (setq projectRoot (projectIDE-get-project-path signature))
      (setq string-list (mapconcat 'wildcard-to-regexp string-list "\\|"))

      (setq remove-folderlist (projectIDE-get-folder-list projectIDE-active-project
                                                          nil
                                                          (lambda (elt)
                                                            (if (string-match string-list elt) t nil))))
      (setq remove-folderlist (cl-remove-if 'projectIDE-cleanup-folder-predicate remove-folderlist))
      
      (setq remove-filelist (projectIDE-get-file-list projectIDE-active-project
                                                      nil
                                                      (lambda (elt)
                                                        (if (string-match string-list elt) t nil))))
      (setq remove-filelist (cl-remove-if 'projectIDE-cleanup-folder-predicate remove-filelist))
      
      
      (mapc (lambda (elt)
              (projectIDE-shadow-copy projectRoot elt cleanup-dir))
            remove-folderlist)
      (mapc (lambda (elt)
              (projectIDE-shadow-copy projectRoot elt cleanup-dir))
            remove-filelist)

      (mapc 'projectIDE-delete-file remove-filelist)
      (mapc 'projectIDE-delete-directory remove-folderlist)

      (projectIDE-message 'Info
                          "Cleanup finished."
                          t
                          (projectIDE-caller 'projectIDE-cleanup)))))



(defun projectIDE-restore-cleanup ()

  "An interactive function to restore cleanup."
  
  (interactive)

  (catch 'Error

    (unless (projectIDE-get-Btrace-signature)
      (projectIDE-message 'Warning
                          "Current buffer is not part of a project."
                          t
                          (projectIDE-caller 'projectIDE-compiler))
      (throw 'Error nil))

    
    (let* ((signature (projectIDE-get-Btrace-signature))
           (projectRoot (projectIDE-get-project-path signature))
           choices
           choice)

      (if (file-exists-p (concat projectRoot PROJECTIDE-CLEANUP-FOLDER))
          (setq choices (cl-remove-if
                         (lambda (elt) (if (or (string= "." elt) (string= ".." elt)) t nil))
                         (directory-files (concat projectRoot PROJECTIDE-CLEANUP-FOLDER))))
        (projectIDE-message 'Info
                            "There is no cleanup history"
                            t
                            (projectIDE-caller 'projectIDE-compiler))
        (throw 'Error nil))

      (if choices
          (setq choice (projectIDE-prompt "Choose cleanup to restore: " choices nil t))
        (projectIDE-message 'Info
                            "There is no cleanup history"
                            t)
        (throw 'Error nil))
    
    (projectIDE-restore-directory (concat
                                   projectRoot
                                   (file-name-as-directory PROJECTIDE-CLEANUP-FOLDER)
                                   (file-name-as-directory choice))
                                  projectRoot))))

(provide 'projectIDE-cleanup)
;;; projectIDE-cleanup.el ends here
