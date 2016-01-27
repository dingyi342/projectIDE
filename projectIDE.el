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
  :tag "Projection creation"
  :group 'projectIDE)
(defgroup projectIDE-config-file nil
  "Setting for loading individual project config file."
  :tag "Config file settings"
  :group 'projectIDE)
(defgroup projectIDE-hook nil
  "All available projectIDE hooks."
  :tag "Hook"
  :group 'projectIDE)

;;; Variables
;; Global Effective Variable
(defconst PROJECTIDE-PROJECTROOT-IDENTIFIER ".projectIDE"
  "The root file indicator.")

(defcustom projectIDE-database-path
  (file-name-as-directory
   (concat (file-name-as-directory user-emacs-directory) "projectIDE"))
  "Type: string\nPath for storing projectIDE RECORD database."
  :tag "Main database path"
  :type 'directory
  :group 'projectIDE-global)

(defconst PROJECTIDE-GLOBAL-PROJECT-RECORD-FILE
  (concat projectIDE-database-path "RECORD")
  "Type: string\nFilename for project record.")

(defconst PROJECTIDE-INDIVIDUAL-PROJECT-RECORD-PATH
  (file-name-as-directory (concat projectIDE-database-path "INDIVIDUAL"))
  "Type: string\nFolder path to individual project record.")

;; Runtime Variable
(defvar projectIDE-p nil
  "Indicate whether projectIDE is running.")

(defvar projectIDE-global-project-record nil
  "Database recording all project.
Never attempt to modify it directly.")

(defvar projectIDE-current-opened-project nil
  "Current opened project.
Never attempt to modify it directly.

Type: project list")

;; Project Creation Variable
(defcustom projectIDE-create-defaultDir (getenv "HOME")
  "Global default project directory.
When creating project, if no specific directory or
invalid default directory is entered, projectIDE uses this
variable as the default directory."
  :tag "Default project directory"
  :type 'directory
  :group 'projectIDE-project-creation)

(defcustom projectIDE-global-project-create-hook nil
  "Hook runs when creating project."
  :tag "projectIDE-global-project-create-hook"
  :type 'hook
  :group 'projectIDE-project-creation
  :group 'projectIDE-hook)

(defcustom projectIDE-create-require-confirm t
  "Require confirmation when creating project?
If this value is nil, projectIDE will skip confirmation
before creating project.
Other values ask for the confirmation."
  :tag "Require confirmation when creating project?"
  :type 'boolean
  :group 'projectIDE-project-creation)

;; Project config file variable
(defconst projectIDE-default-config-key
  '("^signature="
    "^name="
    "^include="
    "^exclude="
    "^inject=")
  "Default projectIDE config file keyword.
Must not change.")

(defcustom projectIDE-default-exclude
  '("*.idea*"
    "*.eunit*"
    "*.git*"
    "*.hg*"
    "*.fslckout*"
    "*.bzr*"
    "*_darcs*"
    "*.tox*"
    "*.svn*"
    "*.stack-work*")
  "A list of exclude items by projectIDE."
  :group 'projecIDE-config-file
  :type '(repeat string))

(defcustom projectIDE-default-include
  '("*")
  "A list of include items by projectIDE."
  :group 'projecIDE-config-file
  :type '(repeat string))

(defcustom projectIDE-config-file-search-up-level 4
  "Number of upper level directories to search for the .projectIDE file."
  :tag "Config file search up level"
  :type 'integer
  :group 'projectIDE-config-file)

;; Implement later
(cl-defstruct injector
  file
  replace ;;(####ABCED#### . ####ABCDEEND####)
  source ;; ####FILE#####*.cpp####FILEEND####
  )

(cl-defstruct project
  ;; public
  ;; config file
  signature           ;; string         eg. "173874102"
  name                ;; string         eg. "HelloWorld"
  (include projectIDE-default-include)    ;; string-list    eg. ("src/*.cpp" "inc/*.h")
  (exclude projectIDE-default-exclude)    ;; string-list    eg. ("*.git" ".projectIDE")
  project-open-hook   ;; symbol
  file-open-hook      ;; symbol
  compile-hook        ;; symbol
  injector            ;; injector-list

  ;; create
  file-create-hook    ;; symbol-list    eg. ('fun1 'fun2 'fun3)

  ;; private
  last-file-list-update ;; string-list
  inc-exc-changed
  file-list)

(cl-defstruct record
  signature
  name
  path
  create-date
  last-modify)

(defun projectIDE-configParser (file)
  "Parse .projectIDE config FILE.
Return a 'project' object created by the FILE.

Return
Type:\t\t project object
Descrip.:\t Project object created by parsing FILE.

FILE
Type:\t\t string
Descrip.:\t Flie path to .projectIDE."
  (catch 'parse-error
    (let (key (project (make-project)))

      ;; Combine the projectIDE-default-config-key
      (dolist (val projectIDE-default-config-key)
        (if key
            (setq key (concat val "\\|"key))
          (setq key val)))
      
      (with-temp-buffer
        (insert-file-contents file)
        ;; (message "[ProjectIDE] Config file contains: %s" (buffer-string))
        (while (search-forward-regexp key nil t)
          ;; (message "[ProjectIDE] Found something")
          (save-excursion
            (beginning-of-line)
            (let ((notFound t)
                  (end (line-end-position))
                  (keylist projectIDE-default-config-key)
                  (counter 0))
              (while (and notFound (car-safe keylist))
                (if (search-forward-regexp (car keylist) end t)
                    (progn
                      (cond
                       ((= counter 0) ;; "^signature="
                        (when (project-signature project)
                          (message "[ProjectIDE] Error: Config file corrupt. 'signature' defined more than once.")
                          (throw 'parse-error nil))
                        (setf (project-signature project) (trim-string (buffer-substring-no-properties (point) end))))
                       ((= counter 1) ;; "^name="
                        (when (project-name project)
                          (message "[ProjectIDE] Error: Config file corrupt. 'name' defined more than once.")
                          (throw 'parse-error nil))
                        (setf (project-name project) (trim-string (buffer-substring-no-properties (point) end))))
                       ((= counter 2) ;; "^include="
                        (let ((include-list (project-include project))
                              (value (split-string (buffer-substring-no-properties (point) end))))
                          (when value
                            (setq include-list (append include-list value))
                            (setq include-list (cl-remove-duplicates include-list :test 'string=))
                            (setf (project-include project) include-list))))
                       ((= counter 3) ;; "^exclude="
                        (let ((exclude-list (project-exclude project))
                              (value (split-string (buffer-substring-no-properties (point) end))))
                          (when value
                            (setq exclude-list (append exclude-list value))
                            (setq exclude-list (cl-remove-duplicates exclude-list :test 'string=))
                            (setf (project-exclude project) exclude-list))))
                       ((= counter 4) ;; "^inject"
                        ;; Implement later
                        (message "[ProjectIDE] Inject eature not availiable for this version.")))
                      (setq notFound nil)))
                (setq counter (1+ counter))
                (setq keylist (cdr keylist)))
              ))))
      
      ;; Return value
      project)))


(defun trim-string (string)
  "Return trimmed leading and tailing whitespace from STRING.
If the return string is a null string, return nil instead.

Return
Type:\t\t string or nil
Descrip.:\t Trimmed string or nil if blank string.

STRING
Type:\t\t string
Descrip.:\t String to be trimmed."
  (let ((return-string
         (replace-regexp-in-string (rx (or (: bos (* (any " \t\n")))
                                           (: (* (any " \t\n")) eos)))
                                   ""
                                   string)))
    ;; Return type
    (if (string= return-string "")
        nil
      return-string)))

(defun fout<<projectIDE (file data)
       "Write FILE with DATA.
DATA is any symbol variable in Emacs.

This function is safe.
It checks the accessibilty of file.
Return NIL if file is not accessable.
It checks the data for a valid symbol.
Return NIL if data value is void.
Return t if wirting file successfully

It is designed to simulate C++ std::cout.

Example:
\(if \(file<<data \"~\\Documents\\foo.txt\" 'foo\)
    \(message \"All success.\"\)
  \(message \"Some problem.\"\)\)

Return
Type:\t\t bool
Descrip.:\t t for no error, nil for error.

FILE
Type:\t\t string
Descrip.:\t Path to output file.
Example:\t ~/.emacs.d/file.txt , ~/usr/mola/documents/cache.txt

DATA
Type:\t\t symbol
Descrip.:\t Serialize data holds by symbol."
       
       (catch 'Error
         (let ((noError t))
           ;; Check file accessibility
           (unless (and (file-exists-p file) (file-writable-p file))
             (let ((parentPath (file-name-directory file)))
               (when (or (file-directory-p parentPath)
                         (make-directory parentPath t)
                         (file-directory-p parentPath))
                 (write-region "" nil file nil 'inhibit nil 'exc1))
               (unless (and (file-exists-p file) (file-writable-p file))
                 (throw 'Error nil))))

           ;; Check symbol exists
           (unless (boundp data)
             (throw 'Error nil))

           ;; Serialize and wirte data to file
           (with-temp-file file
             (insert (prin1-to-string (symbol-value data))))
           
           ;; return value
           noError)))

(defun fin>>projectIDE (file symbol)
  "This function is safe.
It read data from FILE and restore data to SYMBOL.

It returns t it there isn't any error
It returns nil if data cannot be restored.

Return
Type:\t\t bool
Descrip.:\t t for no error, nil for error.

FILE
Type:\t\t string
Descrip.:\t Path to input file.
Example:\t ~/.emacs.d/file.txt , ~/usr/mola/documents/cache.txt"
  
  (catch 'Error
    ;; Check file accessibility
    (unless (file-readable-p file)
      (throw 'Error nil))

    ;; Read from file
    (with-temp-buffer
      (insert-file-contents file)
      (unless (equal (point-min) (point-max))
        (if (boundp symbol)
            (set symbol (read (buffer-string)))
          (throw 'Error nil))))
    
    ;; Return value
    t))


(cl-defmacro projectIDE-create (projectType &key templateDir defaultDir document)
  "Create projection creation function."
  (let* ((projectType (symbol-name projectType))
         (templateDir (file-name-as-directory templateDir))
         (defaultDir (file-name-as-directory defaultDir))
         (document (or document (format
                                 "Create new '%s' project.\nProjectIDE will create a new folder named PROJECTNAME under DIR"
                                 projectType))))
    
    ;; Check if the template directory and default directory exist
    (if (and (file-directory-p templateDir)
             (or (file-directory-p defaultDir)
                 (setq defaultDir (or projectIDE-create-defaultDir user-emacs-directory))))
        
        ;; Function template
        `(defun ,(intern (concat "projectIDE-create-" (downcase projectType))) (projectName dir)

           ;; Documentation
           ,document

           ;; Interactive call to ask for project name and project directory
           (interactive (list (read-string "Project Name: ")
                              (read-directory-name "Create project at: " ,defaultDir)))
           (setq dir (file-name-as-directory dir))

           (if (not (string= projectName "")) ;; Null string guard
               (if (and (file-accessible-directory-p dir) (file-writable-p dir)) ;; Invalid project directory guard
                   (let ((projectRoot (concat dir projectName)))
                     (if (or (not projectIDE-create-require-confirm) ;; Confirm project creation guard
                             (y-or-n-p (format "Project\t\t\t\t: %s\nTemplate\t\t\t: %s\nProject Directory\t: %s\nCreate Project ? "
                                               projectName ,templateDir projectRoot)))

                         ;; Create project
                         (progn
                           (make-directory projectRoot)
                           (copy-directory ,templateDir projectRoot nil nil t)
                           (projectIDE-project-root-creator projectRoot)
                           (projectIDE-RECORD-create
                            (concat (file-name-as-directory projectRoot) PROJECTIDE-PROJECTROOT-IDENTIFIER))
                           (projectIDE-individual-record-create
                            (concat (file-name-as-directory projectRoot) PROJECTIDE-PROJECTROOT-IDENTIFIER))
                           
                           (run-hooks 'projectIDE-global-project-create-hook)
                           (message "Project Created\nProject\t\t\t\t: %s\nTemplate\t\t\t: %s\nProject Directory\t: %s"
                                    projectName ,templateDir projectRoot))
                       
                       (message "Projection creation canceled."))) ;; Else of confirm project creation guard
                 (message "Project directory \"%s\" is invalid. Either not exist or non-accessible." dir)) ;; Else of invalid project directory guard
             (message "Project name cannot be empty string."))) ;; Else of Null string guard
      
      (message "Template directory \"%s\" error\nEither not exists, not directory or non-accessible." templateDir))))


(defun projectIDE-project-root-creator (path)
  "Create '.projectIDE' at PATH to indicate a project root.

PATH
Type:\t\t string
Descrip.:\t Path to project root."
  ;; Generate .projectIDE if not exist
  (unless (memq PROJECTIDE-PROJECTROOT-IDENTIFIER (directory-files (file-name-as-directory path)))
    (write-region "" nil (concat (file-name-as-directory path) PROJECTIDE-PROJECTROOT-IDENTIFIER) t 'inhibit))

    (let* ((letfile (concat (file-name-as-directory path) PROJECTIDE-PROJECTROOT-IDENTIFIER))
           (letproject (projectIDE-configParser letfile))
           (letsignature (concat (number-to-string (random most-positive-fixnum))
                                 (number-to-string (random most-positive-fixnum))
                                 (number-to-string (random most-positive-fixnum))))
           (letname (project-name letproject)))
           
      (with-temp-file letfile
        ;; If signature exists in .projectIDE, remove it
        (when (project-signature letproject)
          (while (search-forward-regexp "^signature=" nil t)
            (delete-region (line-beginning-position) (1+ (line-end-position)))))
        ;; If name not exists in projectIDE, create for it
        (unless letname
          (while (search-forward-regexp "^name=" nil t)
            (delete-region (line-beginning-position) (1+ (line-end-position))))
          (setq letname (file-name-nondirectory (directory-file-name (file-name-directory letfile)))))

        ;; Write to .projectIDE
        (goto-char 1)
        (insert "## This file is generated by projectIDE\n"
                "## There are several keys availiable.\n"
                "## You can see documents for all keys.\n"
                "## Keys must start on a newline and end with a '='.\n"
                "signature=" letsignature "\n"
                "## Never create or change the signature manually!!!\n\n"
                "name=" letname "\n"
                "include=" (mapconcat 'identity (project-include letproject) " ") "\n"
                "exclude=" (mapconcat 'identity (project-exclude letproject) " ")"\n"))))

(defun projectIDE-RECORD-create (configfile)
  "Create record by reading CONFIGFILE.
Write to RECORD file afterward."
  (let ((letproject (projectIDE-configParser configfile))
        (letrecord (make-record)))
    (setf (record-signature letrecord)(project-signature letproject)
          (record-name letrecord) (project-name letproject)
          (record-path letrecord) (file-name-directory configfile)
          (record-create-date letrecord) (time-to-days (current-time))
          (record-last-modify letrecord) (time-to-days (current-time)))
    (add-to-list 'projectIDE-global-project-record letrecord)
    (fout<<projectIDE PROJECTIDE-GLOBAL-PROJECT-RECORD-FILE 'projectIDE-global-project-record)))

(defun projectIDE-individual-record-create (configfile)
  "Create individual record by CONFIGFILE."
  (let* ((letproject (projectIDE-configParser configfile))
         (letfile (concat PROJECTIDE-INDIVIDUAL-PROJECT-RECORD-PATH (project-signature letproject))))
        (unless (file-exists-p letfile)
          (write-region "" nil letfile t 'inhibit)
          (fout<<projectIDE letfile 'letproject))))

(defun projdectIDE-individual-record-file-list-update () "DOC.")

(defun projectIDE- ()
  "DOC."
  (let ((foundProject nil)
        (currentBufferPath (file-name-directory (buffer-file-name)))
        (currentProject projectIDE-current-opened-project)
        (currentRecord))
    
    ;; Find project in currnt opened project
    (while (and (not foundProject) (car-safe currentProject))
      (when (string-prefix-p
             (project-path (car currentProject))
             currentBufferPath)
        (setq foundProject t))
      (setq currentProject (cdr currentProject)))

    ;; Search .projectIDE up directory
    (let ((searchCount projectIDE-config-file-search-up-level)
          (path currentBufferPath))
      (while (and (not foundProject) (> searchCount 0))
        (when (file-exists-p (concat path PROJECTIDE-PROJECTROOT-IDENTIFIER))
          (add-to-list 'projectIDE-current-opened-project
                       (projectIDE-configParser (concat path PROJECTIDE-PROJECTROOT-IDENTIFIER)))
          (setq foundProject t))
        (setq searchCount (1- searchCount))
        (setq path (file-name-directory (directory-file-name path)))))
        
        ;; Find in 
    ;;(while (and (not foundProject) (car-sa)))
        ))

          
  
(defun projectIDE-initialize ()
  "ProjectIDE-initialize."
  (interactive)
  (unless (file-exists-p projectIDE-database-path)
    (make-directory projectIDE-database-path))
  (unless (file-exists-p PROJECTIDE-INDIVIDUAL-PROJECT-RECORD-PATH)
    (make-directory PROJECTIDE-INDIVIDUAL-PROJECT-RECORD-PATH))
  (unless (file-exists-p PROJECTIDE-GLOBAL-PROJECT-RECORD-FILE)
    (write-region "" nil PROJECTIDE-GLOBAL-PROJECT-RECORD-FILE t 'inhibit))
  (if (fin>>projectIDE PROJECTIDE-GLOBAL-PROJECT-RECORD-FILE 'projectIDE-global-project-record)
      (progn
        (message "[projectIDE] projectIDE starts successfully.")
        (setq projectIDE-p t))
    (message "[projectIDE] Error!!! projectIDE starts fail.")))

;;;; Testing function
(defvar projectIDE-test-path
  (file-name-as-directory
   (concat
    (file-name-as-directory
     (concat default-directory "projectIDE")) "test")))
(defvar projectIDE-testfile1 (concat projectIDE-test-path "testfile1.txt"))
(defvar projectIDE-testfile2 (concat projectIDE-test-path "testfile2.txt"))
(defvar projectIDE-testfile3 (concat projectIDE-test-path "testfile3.txt"))
(defvar projectIDE-testfile4 (concat projectIDE-test-path "testfile4.txt"))
(defvar projectIDE-testfile5 (concat projectIDE-test-path "testfile5.txt"))
;; (defvar projectIDE-testvar1 '("Hello" "world" "I" "am" "Mola"))
 (defvar projectIDE-testvar1 nil)
;; (defvar projectIDE-testvar2 '(1 '(2 '(3 '(4)))))
(defvar projectIDE-testvar2 nil)
(defvar projectIDE-testvar3 nil)
(defvar projectIDE-testvar4 nil)
(defvar projectIDE-testvar5 nil)

;; (time-to-days (current-time))
;;; Testing file stream
;; (fout<<projectIDE nil projectIDE-testfile1 'projectIDE-testvar1 'projectIDE-testvar2 'sdkf 'jfaksd 'projectIDE-test-path)
;; (fin>>projectIDE projectIDE-testfile1 projectIDE-testfile2 projectIDE-testfile3)
;;;; End Testing
(projectIDE-initialize)
(provide 'projectIDE)
;;; projectIDE.el ends here
