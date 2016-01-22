;;; projectIDE.el --- project configuration file

;; Copyright (C) 2015 Mola-T
;; Author: Mola-T <Mola@molamola.xyz>
;; URL: https://github.com/mola-T/projectIDE
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

;;; Customization
(defgroup projectIDE nil
  "Managing projects like an IDE."
  :tag "projectIDE"
  :group 'tools
  :group 'convenience)
(defgroup projectIDE-global nil
  "Global setting for all projects."
  :tag "Enviroment Values"
  :group 'projectIDE)
(defgroup projectIDE-project-creation nil
  "Setting for creating project."
  :tag "Projection Creation"
  :group 'projectIDE)

;;; Variables
(defcustom projectIDE-create-defaultDir (getenv "HOME")
  "Global default project directory.
When creating project, if no specific directory or
invalid default directory is entered, projectIDE uses this
variable as the default directory."
  :tag "Default project directory"
  :type 'directory
  :group 'projectIDE-project-creation)

(defcustom projectIDE-database-path
  (concat (file-name-as-directory user-emacs-directory) "prjectIDE")
  "Type: string\nPath for storing projectIDE database."
  :tag "Main database path"
  :type 'directory
  :group 'projectIDE-global)

(defcustom projectIDE-create-require-confirm t
  "Require confirmation when creating project?
If this value is t, projectIDE will ask for confirmationbefore creating project.
Other values skip the confirmation."
  :tag "Require confirmation when creating project?"
  :type 'boolean
  :group 'projectIDE-project-creation)

(defun trim-string (str)
  "Trim leading and tailing whitespace from STR.

STR
Type:\t\t string
Descrip.:\t String to be trimmed."
  (replace-regexp-in-string (rx (or (: bos (* (any " \t\n")))
                                    (: (* (any " \t\n")) eos)))
                            ""
                            str))

(defun serialize (data)
  "Return string of serialized DATA."
  (let ((symbol (symbol-name data))
        (value (symbol-value data)))
    ;; (message "Symbol is %s" symbol) ;;;;; Debug
    ;; (message "Data is %s" value) ;;;;; Debug
    (cond
     ((stringp value)
      (with-temp-buffer
        (insert "####" symbol "\n"
                "###s\n"
                "#" value "\n")
        (buffer-string)))
     (t nil)
     (nil nil))))



(defun file<<data (append file data &rest moredata)
       "Write data to file.

APPEND
Type:\t\t boolean
Descrip.:\t If non-nil append, otherwise overwrite.

FILE
Type:\t\t string
Descrip.:\t Path to output file.
Example:\t ~/.emacs.d/file.txt , ~/usr/mola/documents/cache.txt

DATA
Type:\t\t symbol
Descrip.:\t Serialize data holds by symbol.

MOREDATA
Type:\t symbol
Descrip.:\t Same as DATA.  Can serialize multiple symbols."
       (catch 'fileError
         (unless (and (file-exists-p file) (file-writable-p file))
           (let ((parentPath (file-name-directory file)))
             (when (or (file-directory-p parentPath)
                       (make-directory parentPath t)
                       (file-directory-p parentPath))
               (write-region "" nil file nil 'inhibit nil 'exc1))
             (unless (and (file-exists-p file) (file-writable-p file))
               (throw 'fileError "Error writing file!"))))
         (write-region (serialize data) nil file append 'inhibit)))

(defun data<< (file &rest morefile)
  "Read file to data.

FILE
Type:\t\t string
Descrip.:\t Path to input file.
Example:\t ~/.emacs.d/file.txt , ~/usr/mola/documents/cache.txt

MOREFILE
Type:\t\t string
Descrip.:\t Same as FILE.  Can read multiple files."
  (catch 'fileError
    (unless (file-readable-p file)
      (throw 'fileError "Error reading file!"))
    (with-temp-buffer
      (insert-file-contents file)
      (while (search-forward "####" nil t)
        (let (startPoint symbol-name)
          (setq startPoint (point))
          (unless (search-forward "\n" nil t)
            (throw 'fileError (format "File %s corrupted." file)))
          (message (trim-string (buffer-substring startPoint (point))))
          ))))
  )


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
         (templateDir (file-name-as-directory (symbol-name templateDir)))
         (defaultDir (file-name-as-directory (symbol-name defaultDir)))
         (document (or document (format
                                 "Create new %s project.\nProjectIDE will create new folder under 'dir'\nwith folder name 'projectname'"
                                 projectType))))
    
    ;; Check if the template directory and default directory exist
    (if (and (file-directory-p templateDir)
             (or (file-directory-p defaultDir)
                 (setq defaultDir (or projectIDE-create-defaultDir user-emacs-directory))))

        ;; Function template
        `(defun ,(intern (concat "projectIDE-create-" (downcase projectType))) (projectName dir)

           ;; Documentation
           ,document

           ;; Interactive call
           (interactive (list (read-string "Project Name: ")
                              (read-directory-name "Create project at: " ,defaultDir)))
           (setq dir (file-name-as-directory dir))

           (if (not (string= projectName "")) ;; Null string guard
               (if (file-accessible-directory-p dir) ;; Invalid project directory guard
                   (let ((projectRoot (concat dir projectName)))
                     (if (or (not projectIDE-create-require-confirm)
                             (y-or-n-p (format "Project\t\t\t\t: %s\nTemplate\t\t\t: %s\nProject Directory\t: %s\nCreate Project ? "
                                               projectName ,templateDir projectRoot)))
                         ;; Create project
                         (progn
                           (mkdir projectRoot)
                           (copy-directory ,templateDir projectRoot nil nil t)
                           (message "Project Created\nProject\t\t\t\t: %s\nTemplate\t\t\t: %s\nProject Directory\t: %s"
                                    projectName ,templateDir projectRoot))
                       (message "Projection creation canceled.")))
                 (message "Project directory \"%s\" is invalid. Either not exist or non-accessible." dir)) ;; Else of invalid project directory guard
             (message "Project name cannot be empty string."))) ;; Else of Null string guard

      ;; Prevent endless loop if any error in function prototype
      `(defun ,(intern (concat "projectIDE-create-" (downcase (symbol-name projectType)))) ()
         (interactive)
         (message "Function invalid."))
      (message "Template directory \"%s\" error\nEither not exists, not directory or non-accessible." templateDir))))

(defun projectIDE-initialize ()
  "Document."
  (interactive)
  (message "Directory directory: %s" (concat (file-name-as-directory projectIDE-database-path) "cache"))
  )


;;;; Testing function
;;; Testing file stream
(setq testsymbol "I am foo.")
(setq testfile "~/.emacs.d/elpa/projectIDE/test/mytest.txt")
;; (serialize 'testfile)
;; (file<<data nil testfile 'testsymbol)
;; (data<< testfile)
;; (with-temp-file testfile (insert "hello"))
;; (with-temp-file testfile (goto-char (point-max))(insert "world"))
;; (write-region "hello" nil testfile nil 'excl)
;; (write-region "world" nil testfile t 'excl)
;; (insert "abc")
;;;; End Testing

(provide 'projectIDE)
;;; projectIDE.el ends here
