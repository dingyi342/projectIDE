;;; projectIDE.el --- project configuration file

;; Copyright (C) 2015 Mola Tang
;; Author: Mola Tang <Mola@molamola.xyz>
;; URL:
;; Version: 0.1
;; Package-Requires: ((cl-lib.el "0.5"))
;; Keywords: project, convenience

;;; License:
;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;; HelloWorld

;;; code:
(require 'cl-lib)

(defconst projectIDE-default-project-directory
  "~/"
  "The default project directory.
It works as the last guard of empty default directory.
So this value does not suppose to be changed.")

(defcustom projectIDE-create-require-confirm t
  "Require confirmation when creating project?
If this value is t, projectIDE will ask for confirmationbefore creating project.
Other values skip the confirmation."
  :group 'projectIDE
  :type 'boolean)


;; A prototype for the real function
;; Provide on-demand creation of the real marco
(cl-defmacro projectIDE-create-prototype (projectType &key templateDir defaultDir document)
  "Prototype of macro 'projectIDE-create'.
This macro does nothing but declare a prototype function.
The prototype function expands itself upon real call.
So it exists for enchancing startup speed."
  `(defun ,(intern (concat "projectIDE-create-" (downcase (symbol-name projectType)))) ()
     (interactive)
     (projectIDE-create ,(intern (symbol-name projectType))
                         :templateDir ,(intern (symbol-name templateDir))
                         :defaultDir ,(intern (symbol-name defaultDir))
                         :document ,(intern (symbol-name document)))
     (call-interactively (quote ,(intern (concat "projectIDE-create-" (symbol-name projectType)))))))

;; Real create project marco
(cl-defmacro projectIDE-create (projectType &key templateDir defaultDir document)
  "Create projection creation function."
  (let* ((projectType (symbol-name projectType))
         (templateDir (directory-file-name (symbol-name templateDir)))
         (defaultDir (directory-file-name (symbol-name defaultDir)))
         (document (or document (format
                                 "Create new %s project.\nProjectIDE will create new folder under 'dir'\nwith folder name 'projectname'"
                                 projectType))))
    
    ;; Check if the template directory and default directory exist
    (if (and (file-directory-p templateDir)
             (or (file-directory-p defaultDir)
                 (setq defaultDir projectIDE-default-project-directory)))

        ;; Function template
        `(defun ,(intern (concat "projectIDE-create-" (downcase projectType))) (projectName dir)

           ;; Documentation
           ,document

           ;; Interactive call
           (interactive (list (read-string "Project Name: ")
                              (read-directory-name "Create project at: " ,defaultDir)))
           (setq dir (directory-file-name dir))

           (if (not (string= projectName "")) ;; Null string guard
               (if (file-directory-p dir) ;; Invalid project directory guard
                   (let ((projectRoot (concat dir "/" projectName)))
                     (if (or (not projectIDE-create-require-confirm)
                             (y-or-n-p (format "Project\t\t\t\t: %s\nTemplate\t\t\t: %s\nProject Directory\t: %s\nCreate Project ? "
                                               projectName ,templateDir projectRoot)))
                         (progn
                           (mkdir projectRoot)
                           (copy-directory ,templateDir projectRoot nil nil t)
                           (message "Project Created\nProject\t\t\t\t: %s\nTemplate\t\t\t: %s\nProject Directory\t: %s"
                                    projectName ,templateDir projectRoot))
                       (message "Projection creation canceled")))
                 (message "Project directory \"%s\" is invalid." dir)) ;; Else of invalid project directory guard
             (message "Project name cannot be empty string"))) ;; Else of Null string guard
      
      (message "Template directory \"%s\" error\nEither not exists, not directory or not accessible." templateDir))))
  
(provide 'projectIDE)
;;; projectIDE.el ends here
