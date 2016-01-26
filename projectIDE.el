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

(defcustom projectIDE-global-record-path
  (concat (file-name-as-directory user-emacs-directory) "projectIDE")
  "Type: string\nPath for storing projectIDE database."
  :tag "Main database path"
  :type 'directory
  :group 'projectIDE-global)

(defconst projectIDE-global-project-record-filename "REDORD"
  "Type: string\nFilename for project record.")

(defconst projectIDE-individual-project-record-path
  (concat (file-name-as-directory projectIDE-global-record-path) "INDIVIDUAL")
  "Type: string\nFolder path to individual project record.")

;; Runtime Variable
(defvar projectIDE-global-project-record nil
  "Database recording all project.
Never attempt to modify it directly.")

;; Project Creation Variable
(defcustom projectIDE-create-defaultDir (getenv "HOME")
  "Global default project directory.
When creating project, if no specific directory or
invalid default directory is entered, projectIDE uses this
variable as the default directory."
  :tag "Default project directory"
  :type 'directory
  :group 'projectIDE-project-creation)

(defcustom projectIDE-create-hook nil
  "Hook runs when creating project."
  :tag "projectIDE-create-hook"
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
  signature ;; string         eg. "173874102"
  name      ;; string         eg. "HelloWorld"
  include   ;; string-list    eg. ("src/*.cpp" "inc/*.h")
  exclude   ;; string-list    eg. ("*.git" ".projectIDE")
  injector  ;; injector-list

  ;; create
  hook      ;; symbol-list    eg. ('fun1 'fun2 'fun3)

  ;; private
  ;; 
  create    ;; integer        eg. 1234
  ;; 
  file-list)

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
        (message "[ProjectIDE] Config file contains: %s" (buffer-string))
        (while (search-forward-regexp key nil t)
          (message "[ProjectIDE] Found something")
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
                              (value (trim-string (buffer-substring-no-properties (point) end))))
                          (when value
                            (add-to-list 'include-list value)
                            (setf (project-include project) include-list))))
                       ((= counter 3) ;; "^exclude="
                        (let ((exclude-list (project-exclude project))
                              (value (trim-string (buffer-substring-no-properties (point) end))))
                          (when value
                            (add-to-list 'exclude-list value)
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

(defun projectIDE-dataSerializer (data)
  "This is a projectIDE internal function.
Return a single string of serialized DATA.
The string begins with a start flag \\n#### and the symbol name.
The string ends with an end flag \\n##END###.
The real data is in the middle.

Example:
\t\(setq foo '\(\"Hello\" \"World\"\)\)
\t\(projectIDE-dataSerializer 'foo\)
\t=>\t\"
\t\t####foo
\t\t\(\"Hello\" \"World\"\)
\t\t###END###\"

Return
Type:\t\t string
Descrip.:\t Single serialized string.

DATA
Type:\t\t symbol
Descrip.:\t Any symbol with ascii data."
  (let ((symbol (symbol-name data))
        (value (symbol-value data)))
    (with-temp-buffer
      (insert "\n####" symbol "\n")
      (insert (prin1-to-string value))
      (insert "\n###END###")
      (buffer-string))))


(defun fout<<projectIDE (append file data &rest moredata)
       "Write data to file.

This function is safe.
It checks the accessibilty of file.
Return NIL if file is not accessable.
It checks the data for a valid symbol.
Return NIL if data value is void.
It checks each of the moredata for a valid symbol.
If any candidate consists invalid symbol, NIL is return.
However, invalid symbol found in moredata will not terminate
the output stream.  The invalid symbol is ignored instead.
Error message will be printed and proceed to next candidate.

It is designed to simulate C++ std::cout.

Example:
\(if \(file<<data nil \"~\\Documents\\foo.txt\" 'foo 'bar 'hello 'world\)
    \(message \"All success.\"\)
  \(message \"Some problem.\"\)\)

Return
Type:\t\t bool or list
Descrip.:\t t for no error.
\t\t\t nil for error at writing first data to file
\t\t\t a list for ignored error in more data

APPEND
Type:\t\t bool
Descrip.:\t If non-nil append, otherwise overwrite.

FILE
Type:\t\t string
Descrip.:\t Path to output file.
Example:\t ~/.emacs.d/file.txt , ~/usr/mola/documents/cache.txt

DATA
Type:\t\t symbol
Descrip.:\t Serialize data holds by symbol.

MOREDATA
Type:\t\t symbol
Descrip.:\t Same as DATA.  Can serialize multiple symbols.
\t\t\t MOREDATA will append to FILE no matter what APPEND is."
       (catch 'Error

         ;; Check file accessibility
         (unless (and (file-exists-p file) (file-writable-p file))
           (let ((parentPath (file-name-directory file)))
             (when (or (file-directory-p parentPath)
                       (make-directory parentPath t)
                       (file-directory-p parentPath))
               (write-region "" nil file nil 'inhibit nil 'exc1))
             (unless (and (file-exists-p file) (file-writable-p file))
               (message "Error writing file!")
               (throw 'Error nil))))

         ;; Serialize first entry
         (unless (boundp data)
           (message "Symbol '%s' is not defined." (symbol-name data))
           (throw 'Error nil))
         (write-region (projectIDE-dataSerializer data) nil file append 'inhibit)

         ;; Serialize all other entries
         (let ((noError t))
           (while (car-safe moredata)
             (if (boundp (car moredata))
                 (write-region (projectIDE-dataSerializer (car moredata)) nil file t 'inhibit)
               (when (equal noError t) (setq noError nil))
               (setq noError (append (list (symbol-name (car moredata))) noError)))
             (setq moredata (cdr moredata)))

           ;; return value
           noError)))

(defun projectIDE-dataParser (file)
  "This is a projectIDE internal function.
It parse FILE which consist projectIDE data
produced by projectIDE-dataSerializer.
The file may have mulitple symbols and corresponding value.
If symbol has already been defined in Emacs,
projectIDE-dataParser will restore it.
If symbol has not yet defined in Emacs,
projectIDE-dataParser will ignore it.
If there is any format error of the FILE,
projectIDE-dataParser will parse up to the error point
and terminate the parsing process.
projectIDE-dataParser returns t if it can parse the whole file.
It returns nil if there is format error.
Or it returns a list for ignored symbol.

Return
Type:\t\t list or bool
Descrip.:\t t for no error.
\t\t\t nil for failing to load data from first FILE
\t\t\t a list for ignored symbols or files

FILE
Type:\t\t string
Descrip.: file path"
  
  (catch 'Error
    (let ((noError t))
      (with-temp-buffer
        (insert-file-contents file)
        (while (search-forward "####" nil t)
          (let (startPoint endPoint name value)
            (setq startPoint (point))
            (unless (search-forward "\n" nil t)
              (message "File '%s' corrupted." file)
              (throw 'Error nil))
            (setq endPoint (point))
            (when (or (search-backward "###" startPoint t) (search-backward " " startPoint t))
              (message "File '%s' corrupted." file)
              (throw 'Error nil))
            (setq name (trim-string (buffer-substring-no-properties startPoint endPoint)))
            (setq startPoint endPoint)
            (unless (search-forward "###END###" nil t)
              (message "File '%s' corrupted." file)
              (throw 'Error nil))
            (setq endPoint (point))
            (save-excursion
              (backward-char 9)
              (when (search-backward "###" startPoint t)
                (message "File '%s' corrupted." file)
                (throw 'Error nil))
              (setq value (trim-string (buffer-substring-no-properties startPoint (point)))))

            ;; Check symbol. If exists, import data.
            (if (boundp (intern name))
                (set (intern name) (read value))
              (when (equal noError t) (setq noError nil))
              (setq noError (cons name noError))))))
      
    ;; Return type
    noError)))

(defun fin>>projectIDE (file &rest morefile)
  "This function is safe.
It reads data from FILE and call projectIDE-dataParser
to restore the data.
It accepts more than one files by MOREFILE.
It returns t it there isn't any error,
returns nil if the first FILE cannot load successfully,
and returns a list for any ignored files/symbols.

Return
Type:\t\t bool or list
Descrip.:\t t for no error.
\t\t\t nil for file format error
\t\t\t a list for ignored symbols

FILE
Type:\t\t string
Descrip.:\t Path to input file.
Example:\t ~/.emacs.d/file.txt , ~/usr/mola/documents/cache.txt

MOREFILE
Type:\t\t string
Descrip.:\t Same as FILE.  Can read multiple files."

  ;; Read first file
  (let ((noError t))
    (catch 'Error

      ;; Check file accessibility
      (unless (file-readable-p file)
        (message "Error reading file!")
        (throw 'Error nil))

      (setq noError (projectIDE-dataParser file)))

    (unless noError
      (throw 'Error nil))

    (while (car-safe morefile)
      (let ((file (car morefile)))
        (if (file-readable-p file)
            (let ((return-value (projectIDE-dataParser file)))
              (if return-value
                  (unless (equal return-value t)
                    (and (equal noError t)(setq noError nil))
                    (setq noError (append return-value noError)))
                (and (equal noError t)(setq noError nil))
                (setq noError (append (list file) noError))))
          (and (equal noError t)(setq noError nil))
          (setq noError (append (list file) noError))))
      (setq morefile (cdr morefile)))
    
    ;; Return value
    noError))


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
               (if (file-accessible-directory-p dir) ;; Invalid project directory guard
                   (let ((projectRoot (concat dir projectName)))
                     (if (or (not projectIDE-create-require-confirm)
                             (y-or-n-p (format "Project\t\t\t\t: %s\nTemplate\t\t\t: %s\nProject Directory\t: %s\nCreate Project ? "
                                               projectName ,templateDir projectRoot)))
                         ;; Create project
                         (progn
                           (make-directory projectRoot)
                           (copy-directory ,templateDir projectRoot nil nil t)
                           (projectIDE-project-root-creator projectRoot)
                           (run-hooks 'projectIDE-create-hook)
                           (message "Project Created\nProject\t\t\t\t: %s\nTemplate\t\t\t: %s\nProject Directory\t: %s"
                                    projectName ,templateDir projectRoot))
                       (message "Projection creation canceled.")))
                 (message "Project directory \"%s\" is invalid. Either not exist or non-accessible." dir)) ;; Else of invalid project directory guard
             (message "Project name cannot be empty string."))) ;; Else of Null string guard
      
      (message "Template directory \"%s\" error\nEither not exists, not directory or non-accessible." templateDir))))


(defun projectIDE-project-root-creator (path)
  "Create '.projectIDE' at PATH to indicate a project root.
Generate project signature if required.

PATH
Type:\t\t string
Descrip.:\t Path to project root."
    (unless (memq PROJECTIDE-PROJECTROOT-IDENTIFIER (directory-files (file-name-as-directory path)))
      (write-region "" nil (concat (file-name-as-directory path) PROJECTIDE-PROJECTROOT-IDENTIFIER) t 'inhibit))

    (let* ((file (concat (file-name-as-directory path) PROJECTIDE-PROJECTROOT-IDENTIFIER))
           (project (projectIDE-configParser file))
           (signature (concat (number-to-string (random most-positive-fixnum))
                              (number-to-string (random most-positive-fixnum))))
           (name )
          ;; Cannot find signature
      (with-temp-file file
        (when (project-signature project)
          (while (search-forward-regexp "^signature=" nil t)
            (delete-region (line-beginning-position) (line-end-position))))
        (let ()
              name
              creation
              path)
        (insert "##Never create or change the signature manually!\n"
                "signature:" signature "\n")
        (if (search-forward-regexp "name:" nil t)
            (message "Generated signature but name can find.")
          (progn
            (setq name (file-name-nondirectory (directory-file-name (file-name-directory file))))
            (goto-char 1)
            (insert "name:" name "\n")
            (setq day (time-to-days (current-time)))
            (add-to-list 'projectIDE-global-project-record (list signature name day)))))))
    
(defun projectIDE-initialize ()
  "ProjectIDE-initialize."
  (interactive)
  (let* ((file (concat (file-name-as-directory projectIDE-global-record-path)
                       projectIDE-global-project-record-filename)))
    (unless (file-exists-p projectIDE-individual-project-record-path)
      (make-directory projectIDE-individual-project-record-path))
    (unless (file-exists-p file)
      (write-region "" nil file t 'inhibit))
    (fin>>projectIDE file)))

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
