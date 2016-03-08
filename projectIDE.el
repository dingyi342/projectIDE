
;;; projectIDE.el --- project configuration file
;;
;; Copyright (C) 2015 Mola-T
;; Author: Mola-T <Mola@molamola.xyz>
;; URL: https://github.com/mola-T/projectIDE
;; Version: 1.0
;; Package-Requires: ((cl-lib.el "0.5") (fdex.el "1.0"))
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
;;; Core function (not intend to be visble to user)
;;; Config file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; (defun projectIDE-parse-config (file &optional errormessage caller))

;;; code:
(require 'cl-lib)
(require 'fdex)
(require 'projectIDE-header)
(require 'projectIDE-debug)
(require 'projectIDE-fstream)





;;; Config file function ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;fff (defun projectIDE-parse-config (file &optional errormessage caller))
(defun projectIDE-parse-config (file &optional errormessage caller)
  "Parse .projectIDE config FILE.

Return a projectIDE-project object created by the FILE.
If .projectIDE is a blank file, return a default projectIDE-project object.
If there is any problem parsing the .projectIDE file, return nil.

Return
Type:\t\t projectIDE-project object or nil
Descrip.:\t Project object created by parsing FILE.
\t\t\t: nil for any error.

FILE
Type:\t\t string
Descrip.:\t Flie path to .projectIDE.

ERRORMESSAGE
Type:\t\t bool
Descrip.:\t Display error message to minibuffer if it is t.

CALLER
Type:\t\t symbol list
Descrip.:\t Function list calling this function for debug purpose."
  (catch 'parse-error
    (let ((project (make-projectIDE-project)))
      
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char 1)

        ;; Search keys
        (while (search-forward-regexp projectIDE-config-key-string nil t)
          (save-excursion

            ;; Identify key
            (beginning-of-line)
            (let ((line-end (line-end-position))
                  (keylist projectIDE-config-key)
                  (counter 0)
                  found)
              (while (and (not found) (car keylist))
                (if (search-forward-regexp (car keylist) line-end t)
                    (progn
                      (cond
                       
                       ((= counter 0) ;; "^signature="
                        (when (projectIDE-project-signature project)
                          (projectIDE-message-handle 'Error
                                                     (format "Config file corrupt. 'signature' in %s definded more than once." file)
                                                     errormessage
                                                     'projectIDE-parse-config
                                                     (nconc (list 'projectIDE-parse-config) caller))
                          (throw 'parse-error nil))
                        (setf (projectIDE-project-signature project) (projectIDE-trim-string (buffer-substring-no-properties (point) line-end))))
                       
                       ((= counter 1) ;; "^name="
                        (when (projectIDE-project-name project)
                          (projectIDE-message-handle 'Error
                                                     (format "Config file corrupt. 'name' in %s definded more than once." file)
                                                     errormessage
                                                     'projectIDE-parse-config
                                                     (nconc (list 'projectIDE-parse-config) caller))
                          (throw 'parse-error nil))
                        (setf (projectIDE-project-name project) (projectIDE-trim-string (buffer-substring-no-properties (point) line-end))))

                       ((= counter 2) ;; "^exclude="
                        (let ((exclude-list (split-string (buffer-substring-no-properties (point) line-end))))
                          (setf (projectIDE-project-exclude project)
                                (projectIDE-append (projectIDE-project-exclude project) exclude-list))))

                       ((= counter 3) ;; "^whitelist="
                        (let ((whitelist (split-string (buffer-substring-no-properties (point) line-end))))
                          (setf (projectIDE-project-whitelist project)
                                (projectIDE-append (projectIDE-project-whitelist project) whitelist))))

                       ((= counter 4) ;; "^cachemode="
                        (when (projectIDE-project-cachemode project)
                          (projectIDE-message-handle 'Error
                                                     (format "Config file corrupt. 'cachemode' in %s definded more than once." file)
                                                     errormessage
                                                     'projectIDE-parse-config
                                                     (nconc (list 'projectIDE-parse-config) caller))
                          (throw 'parse-error nil)))
                       ((= counter 5) ;; "^module="
                        ;; not implemented
                        ))
                        
                      (setq found t))
                (setq counter (1+ counter))
                (setq keylist (cdr keylist))))))))

      (unless (projectIDE-project-exclude project)
        (setf (projectIDE-project-exclude project) projectIDE-default-exclude))

      (unless (projectIDE-project-whitelist project)
        (setf (projectIDE-project-whitelist project) projectIDE-default-whitelist))

      (unless (projectIDE-project-cachemode project)
        (setf (projectIDE-project-cachemode project) projectIDE-default-cachemode))

      (when projectIDE-debug-mode
        (projectIDE-message-handle 'Info
                                   (format "Parsed config file %s successfully" file)
                                   nil
                                   'projectIDE-parse-config
                                   (nconc (list 'projectIDE-parse-config) caller)))
      
      project)))

;;; Config file function ends ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



















;; Indexing function ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;fff (defun projectIDE-generate-signature (&optional caller))
;;fff (defun projectIDE-new-project (path &optional caller))
;;fff (defun projectIDE-root-create (path &optional caller))
;;fff (defun projectIDE-record-create (configfile &optional caller))
;;fff (defun projectIDE-cache-create (configfile &optional caller))
;;fff (defun projectIDE-identify-project (&optional buffer caller))
;;fff (defun projectIDE-manipulate-filter (projectRoot list))
;;fff (defun projectIDE-index-project (path &optional caller))


(defun projectIDE-generate-signature (&optional caller)
  "Generate and return a projectIDE signature.

CALLER is the function list calling this function.
It is uesed for debugging purpose.

Return
Type:\t\t string
Descrip.:\t A signature string.

CALLER
Type:\t\t symbol list
Descrip.:\t Function list calling this function for debug purpose."
  
  (let ((return "")
        (charset "1234567890abcdefghijklmnopqrstyvwxyzABCDEFGHIJKLMNOPQRSTYVWXYZ"))
    (dotimes (i 32)
      (setq return (concat return (char-to-string (elt charset (random (length charset)))))))

    (when projectIDE-debug-mode
      (projectIDE-message-handle 'Info
                                 (format "Generated signature %s" return)
                                 nil
                                 'projectIDE-generate-signature
                                 (nconc (list 'projectIDE-generate-signature) caller)))
    return))


(defun projectIDE-new-project (path &optional caller)
  ;; Ensure PATH is a directory before passing to this function.
  ;; ie. use file-name-as-directory
  "Create project with PATH as project root.
It is a encapsulation of project creation chain.
CALLER is the function list calling this function.
It is uesed for debugging purpose.

PATH
Type:\t\t string
Descrip.:\t Path to project root.

CALLER
Type:\t\t symbol list
Descrip.:\t Function list calling this function for debug purpose."

  (let ((project-config (concat path PROJECTIDE-PROJECTROOT-IDENTIFIER)))
    ;; Generate .projectIDE file
    (projectIDE-root-create path
                            (and projectIDE-debug-mode
                                 (nconc (list 'projectIDE-new-project) caller)))
    ;; Create global record
    (projectIDE-record-create project-config
                              (and projectIDE-debug-mode
                                   (nconc (list 'projectIDE-new-project) caller)))
    ;; Create individual record
    (projectIDE-cache-create project-config
                             (and projectIDE-debug-mode
                                  (nconc (list 'projectIDE-new-project) caller)))))

(defun projectIDE-root-create (path &optional caller)
  "Create '.projectIDE' at PATH to indicate a project root.
Create basic key like signature, name, exclude and whitelist as well.
If '.projectIDE' has already existed, try to read the '.projectIDE config
and give it a new signature.

CALLER is the function list calling this function.
It is uesed for debugging purpose.

PATH
Type:\t\t string
Descrip.:\t Path to project root.

CALLER
Type:\t\t symbol list
Descrip.:\t Function list calling this function for debug purpose."

  ;; Generate .projectIDE if not exist
  (unless (memq PROJECTIDE-PROJECTROOT-IDENTIFIER (directory-files path))
    (write-region "" nil (concat path PROJECTIDE-PROJECTROOT-IDENTIFIER) t 'inhibit))

  (let* ((file (concat path PROJECTIDE-PROJECTROOT-IDENTIFIER))
         (project (projectIDE-parse-config file nil (and projectIDE-debug-mode
                                                         (nconc (list 'projectIDE-root-creator) caller))))
         (signature (projectIDE-generate-signature (and projectIDE-debug-mode
                                                         (nconc (list 'projectIDE-root-creator) caller))))
         (name (projectIDE-project-name project))
         (exclude (projectIDE-project-exclude project))
         (whitelist (projectIDE-project-whitelist project)))
    
    (with-temp-file file
      ;; If signature exists in .projectIDE, remove it
      (when (projectIDE-project-signature project)
        (while (search-forward-regexp "^signature=" nil t)
          (delete-region (line-beginning-position) (line-end-position))))

      (goto-char 1)
      ;; Write to .projectIDE
      ;; Handle signature
      (insert "## This file is generated by projectIDE\n"
              "## There are several keys availiable.\n"
              "## You can see documentation for all keys.\n"
              "## Keys must start on a newline and end with a '='.\n"
              "## Below is an example for key 'signature.\n"
              "signature=" signature "\n"
              "## Signature is unique for each project.\n"
              "## ProjectIDE used the signature to trace every data on that project.\n"
              "## So never create or change the signature manually!!!\n\n")
      ;; Handle name
      ;; If name not exists in projectIDE, create for it
      (unless name
        (setq name (file-name-nondirectory (directory-file-name (file-name-directory file))))
        (insert "name=" name "\n"))
      ;; Handle exclude
      (insert "exclude=" (mapconcat 'identity (projectIDE-project-exclude project) " ") "\n")
      (save-excursion
        (while (search-forward-regexp "^exclude=" nil t)
          (delete-region (line-beginning-position) (line-end-position))))
      ;; Handle whitelist
      (insert "whitelist=" (mapconcat 'identity (projectIDE-project-whitelist project) " ") "\n")
      (save-excursion
        (while (search-forward-regexp "^whitelist=" nil t)
          (delete-region (line-beginning-position) (line-end-position)))))

    (when projectIDE-debug-mode
      (projectIDE-message-handle 'Info
                                 (format "Project root created at %s" path)
                                 nil
                                 'projectIDE-root-creator
                                 (nconc (list 'projectIDE-root-creator) caller)))))

(defun projectIDE-record-create (configfile &optional caller)
  "Create projectIDE-record by reading CONFIGFILE.
Write to RECORD file afterward.

CALLER is the function list calling this function.
It is uesed for debugging purpose.

CONFIGFILE
Type:\t\t string
Descrip.:\t A string of path to .projectIDE config file.

CALLER
Type:\t\t symbol list
Descrip.:\t Function list calling this function for debug purpose."
  (let ((project (projectIDE-parse-config configfile nil
                                          (and projectIDE-debug-mode
                                                         (nconc (list 'projectIDE-record-create) caller))))
        (record (make-projectIDE-record)))
    (setf (projectIDE-record-signature record)(projectIDE-project-signature project)
          (projectIDE-record-name record)(projectIDE-project-name project)
          (projectIDE-record-path record) (file-name-directory configfile)
          (projectIDE-record-create-time record) (current-time)
          (projectIDE-record-last-open record) (current-time))
    (puthash (projectIDE-project-signature project) record projectIDE-runtime-record)
    (fout<<projectIDE PROJECTIDE-RECORD-FILE 'projectIDE-runtime-record (and projectIDE-debug-mode
                                                                             (nconc (list 'projectIDE-record-create) caller))))

  (when projectIDE-debug-mode
    (projectIDE-message-handle 'Info
                               (format "Project record created for %s" configfile)
                               nil
                               'projectIDE-record-create
                               (nconc (list 'projectIDE-record-create) caller))))

(defun projectIDE-cache-create (configfile &optional caller)
  "Create cache by CONFIGFILE.

CALLER is the function list calling this function.
It is uesed for debugging purpose.

CONFIGFILE
Type:\t\t string
Descrip.:\t A string of path to .projectIDE config file.

CALLER
Type:\t\t symbol list
Descrip.:\t Function list calling this function for debug purpose."
  (let* ((cache (make-projectIDE-cache))
         (project (projectIDE-parse-config configfile nil (and projectIDE-debug-mode
                                                               (nconc (list 'projectIDE-cache-create) caller))))
         (signature (projectIDE-project-signature project))
         (projectRoot (projectIDE-record-path (gethash signature projectIDE-runtime-record)))
         (file (concat PROJECTIDE-CACHE-PATH signature))
         (exclude (projectIDE-project-exclude project))
         (exclude-modified (projectIDE-manipulate-filter projectRoot exclude))
         (whitelist (projectIDE-project-whitelist project))
         (whitelist-modified (projectIDE-manipulate-filter projectRoot whitelist)))

    (setf (projectIDE-cache-project cache) project
          (projectIDE-cache-exclude cache) exclude
          (projectIDE-cache-whitelist cache) whitelist
          (projectIDE-cache-file-cache cache) (fdex-new projectRoot exclude-modified whitelist-modified)
          (projectIDE-cache-config-update-time cache) (current-time))
    
    (fout<<projectIDE file 'cache (and projectIDE-debug-mode
                                       (nconc (list 'projectIDE-record-create) caller))))
  
  (when projectIDE-debug-mode
    (projectIDE-message-handle 'Info
                               (format "Project cache created for %s" configfile)
                               nil
                               'projectIDE-cache-create
                               (nconc (list 'projectIDE-cache-create) caller))))

(defun projectIDE-identify-project (&optional buffer caller)
  ;; If buffer is not provided, it implies this function is possibily call by find-file-hook
  ;; With buffer provided it means this function is possibilty call by projectIDE-initialize or projectIDE-index-project
  ;; to check all existing buffers
  "This function check whether BUFFER is a indexed project.


CALLER is the function list calling this function.
It is uesed for debugging purpose.

If it is a indexed project, it ensures
1) the project is under projectIDE-runtime-cache
2) it is under opened buffer in the project cache
3) it is under projectIDE-runtime-Btrace
If BUFFER is not provided, current buffer is used.

BUFFER
Type\t\t: buffer
Descrip.:\t The buffer being identified.

CALLER
Type:\t\t symbol list
Descrip.:\t Function list calling this function for debug purpose."
  
  (let (signature)

    ;; Find project in projectIDE-runtime-cache
    (let ((opened-project (projectIDE-get-all-caching-signature)))
      (while (and (not signature) (buffer-file-name buffer) (car opened-project))
        (when (string-prefix-p (projectIDE-get-project-path (car opened-project)) (buffer-file-name buffer))
          (setq signature (car opened-project)))
        (setq opened-project (cdr opened-project))))
    
    ;; Search in project RECORD
    (unless signature
      (when (buffer-file-name buffer)
        (setq signature (projectIDE-get-signature-by-path (buffer-file-name buffer)))))
    
    ;; Search .projectIDE up directory
    ;; Only apply to call where buffer is not provided
    (unless (or buffer signature)
      (let ((search-countdown projectIDE-config-file-search-up-level)
            (path (file-name-directory (buffer-file-name))))
        (while (and (not signature) (> search-countdown 0))
          (when (file-exists-p (concat path PROJECTIDE-PROJECTROOT-IDENTIFIER))
            (let* ((projectRoot (concat path PROJECTIDE-PROJECTROOT-IDENTIFIER))
                   (project (projectIDE-parse-config projectRoot nil
                                                     (and projectIDE-debug-mode (list 'projectIDE-identify-project 'find-file-hook)))))
              (if (projectIDE-project-signature project)
                  ;; found .projectIDE with signature
                  (progn
                    (setq signature (projectIDE-project-signature project))
                    (projectIDE-record-create projectRoot
                                              (and projectIDE-debug-mode (list 'projectIDE-identify-project 'find-file-hook)))
                    (unless (file-exists-p (concat PROJECTIDE-CACHE-PATH signature))
                      (projectIDE-cache-create projectRoot
                                               (and projectIDE-debug-mode (list 'projectIDE-identify-project 'find-file-hook)))))
                ;; found .projectIDE without signature
                (if (y-or-n-p
                     (projectIDE-message-handle
                      'Info
                      (format ".projectIDE root file found at %s.\nMake this path as project root and index the project? " path)
                      nil
                      'projectIDE-identify-project
                      (and projectIDE-debug-mode (list 'projectIDE-identify-project 'find-file-hook))))
                    (progn
                      (projectIDE-new-project path (and projectIDE-debug-mode (list 'projectIDE-identify-project 'find-file-hook)))
                      (setq signature (projectIDE-get-signature-by-path (buffer-file-name buffer)))
                      (projectIDE-message-handle
                       'Info
                       (format "Project Indexed\nProject\t\t\t\t: %s\nProject Directory\t: %s"
                               (file-name-nondirectory (directory-file-name (file-name-directory projectRoot))) path)
                       t
                       'projectIDE-identify-project
                       (and projectIDE-debug-mode (list 'projectIDE-identify-project 'find-file-hook))))
                  (projectIDE-message-handle 'Info
                                             "File opened without indexing."
                                             t
                                             'projectIDE-identify-project
                                             (and projectIDE-debug-mode (list 'projectIDE-identify-project 'find-file-hook)))
                  (setq search-countdown -1)))))
          
          (setq search-countdown (1- search-countdown)
                path (file-name-directory (directory-file-name path))))))
    
    ;; When buffer is identified,
    ;; adds project to projectIDE-runtime-Btrace
    ;; ensures the project is in projectIDE-runtime-cache
    ;; adds to opened buffer in projectIDE-runtime-cache
    ;; update the last-open time of project record
    (when signature
      (projectIDE-track-buffer signature buffer (and projectIDE-debug-mode
                                                     (nconc (list 'projectIDE-identify-project) (or caller (list 'find-file-hook)))))
      
      (unless buffer
        (projectIDE-message-handle
         'Info
         (format "Opened file from project [%s]" (projectIDE-get-project-name signature))
         t
         'projectIDE-identify-project
         (and projectIDE-debug-mode (nconc (list 'projectIDE-identify-project) (or caller (list 'find-file-hook)))))))

    (when (and projectIDE-debug-mode (not signature))
      (projectIDE-message-handle
       'Info
       (format "Opened buffer '%s' is not a indexed project." (or (buffer-file-name) "invalid buffer"))
       nil
       'projectIDE-identify-project
       (and projectIDE-debug-mode (nconc (list 'projectIDE-identify-project) (or caller (list 'find-file-hook))))))))

(defun projectIDE-manipulate-filter (projectRoot list)
  "This function add the PROJECTROOT as a prefix to each entry in the LIST.
It also ajusts the regexp in the list so that
1) \"*\" is converted to \".*\" to provide wildcard function
2) \".\" is converted to \"\\.\" to prevent misuse of regexp in file extension
3) string end \"\\'\" is added to each list item
This function return a manipulated LIST.

Return
Type:\t\t string list
Descrip.:\t A string list with each item prefixed with PROJECTROOT.

PROJECTROOT
Type:\t\t string
Descrip.:\t A string of path.

LIST
Type:\t\t string list
Descip.:\t A string list which each entry is to be prefixed."

  (let (return)
    (dolist (entry list)
      (projectIDE-add-to-list return
                              (concat projectRoot
                                      (replace-regexp-in-string "\\*" ".*"
                                                     (replace-regexp-in-string "\\." "\\\\." entry))
                                      "\\'")))
    return))

(defun projectIDE-index-project (path &optional caller)
  "This is an interactive function to let user indexing a project.
It will ask for a project root (PATH) and indexing starts there.

CALLER is the function list calling this function.
It is uesed for debugging purpose.

PATH
Type:\t\t string of directory
Descrip.:\t Path to be indexed as project root.

CALLER
Type:\t\t symbol list
Descrip.:\t Function list calling this function for debug purpose."
  
  (interactive (list (read-directory-name "Please choose the project root: "
                                          (file-name-directory (or buffer-file-name user-emacs-directory)))))

  (catch 'Error
    (unless (or projectIDE-p (projectIDE-initialize))
      (projectIDE-message-handle 'Error
                                 "projectIDE not initialized."
                                 t
                                 'projectIDE-index-project
                                 (nconc (list 'projectIDE-index-project) caller))
      (throw 'Error nil))
    
    ;; Index project
    (if (and (file-exists-p (concat path PROJECTIDE-PROJECTROOT-IDENTIFIER))
             (signature (projectIDE-parse-config (concat path PROJECTIDE-PROJECTROOT-IDENTIFIER) nil
                                                 (and projectIDE-debug-mode
                                                      (nconc (list 'projectIDE-index-project) caller)))))
        (if (yes-or-no-p
             (concat (projectIDE-message-handle 'Warning
                                                (format ".projectIDE with signature found at %s" path)
                                                nil 'projectIDE-index-project)
                     "\nChoose yes if you want to create a new signature for this project.
Choose no if you want to retain current signature.
Press C-g to cancel the operation."))
            (projectIDE-new-project (file-name-as-directory path) (and projectIDE-debug-mode
                                                                       (nconc (list 'projectIDE-index-project) caller)))
          (projectIDE-record-create (concat path PROJECTIDE-PROJECTROOT-IDENTIFIER) (and projectIDE-debug-mode
                                                                                         (nconc (list 'projectIDE-index-project) caller)))
          (projectIDE-cache-create (concat path PROJECTIDE-PROJECTROOT-IDENTIFIER) (and projectIDE-debug-mode
                                                                                        (nconc (list 'projectIDE-index-project) caller))))
      (projectIDE-new-project (file-name-as-directory path)
                              (and projectIDE-debug-mode (nconc (list 'projectIDE-index-project) caller))))
    
    ;; Scan through buffers to check whether they are memeber of newly indexed project
    (let ((buffers (buffer-list)))
      (dolist (buffer buffers)
        (projectIDE-identify-project buffer (and projectIDE-debug-mode (nconc (list 'projectIDE-index-project) caller)))))
    
    (projectIDE-message-handle 'Info
                               (format "Project Indexed\nProject\t\t\t\t: %s\nProject Directory\t: %s"
                                       (projectIDE-project-name (projectIDE-parse-config (concat path PROJECTIDE-PROJECTROOT-IDENTIFIER)))
                                       path)
                               t
                               'projectIDE-index-project
                               (and projectIDE-debug-mode (nconc (list 'projectIDE-index-project) caller)))))

;; Indexing function ends ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




















;; Caching function ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;fff (defun projectIDE-config-need-update? (signature &optional caller))
;;fff (defun projectIDE-filter-changed? (signature &optional caller))
;;fff (defun projectIDE-update-project-config (signature &optional ErrorMessage caller))
;;fff (defun projectIDE-update-cache-backend (signature &optional caller))
;;fff (defun projectIDE-update-cache ()) - I
;;fff (defun projectIDE-track-buffer (signature &optional buffer caller))
;;fff (defun projectIDE-untrack-buffer (&optional buffer caller))
;;fff (defun projectIDE-before-save-new-file ())
;;fff (defun projectIDE-after-save-new-file ())
;;fff (defun projectIDE-before-emacs-kill ())


(defun projectIDE-config-need-update? (signature &optional caller)
  "Return t if config file of given SIGNATURE need to be updated.
The config file needs to be updated iif its modification time is
later than the last config update time in cache.

CALLER is the function list calling this function.
It is uesed for debugging purpose.

Return
Type:\t\t bool
Descrip.:\t t if config file needs update, otherwise nil.

SIGNATURE
Type:\t\t string
Descrip.:\t A project based unique ID.

CALLER
Type:\t\t symbol list
Descrip.:\t Function list calling this function for debug purpose."

  (catch 'Error
    (unless (projectIDE-get-cache signature)
      (projectIDE-message-handle 'Warning
                                 (format "Project cache for %s not in projectIDE-runtime-cache." signature)
                                 nil
                                 'projectIDE-config-need-update?
                                 (and projectIDE-debug-mode (nconc (list 'projectIDE-config-need-update?) caller)))
      (throw 'Error nil))
    
    (let ((config (projectIDE-get-config-file-path signature)))
      (if config
          (if (time-less-p (projectIDE-get-config-update-time signature)(fdex-modify-time config))
              t
            nil)

        (when projectIDE-debug-mode
          (projectIDE-message-handle 'Warning
                                     (format "Unable to find project config file for %s" signature)
                                     nil
                                     'projectIDE-config-need-update?
                                     (nconc (list 'projectIDE-config-need-update?) caller)))
        nil))))

(defun projectIDE-filter-changed? (signature &optional caller)
  "Return t if exclude or whitelist changed in project given by SIGNATURE.

CALLER is the function list calling this function.
It is uesed for debugging purpose.

Return
Type:\t\t bool
Descri.:\t t if either exclude or whitelist had changed.

SIGNATURE
Type:\t\t string
Descrip.:\t A project based unique ID.

CALLER
Type:\t\t symbol list
Descrip.:\t Function list calling this function for debug purpose."

  (catch 'Error
    (unless (projectIDE-get-cache signature)
      (projectIDE-message-handle 'Warning
                                 (format "Project cache for %s not in projectIDE-runtime-cache." signature)
                                 nil
                                 'projectIDE-filter-changed?
                                 (and projectIDE-debug-mode (nconc (list 'projectIDE-filter-changed?) caller)))
      (throw 'Error nil))
    
    (if (and (equal (projectIDE-get-project-exclude signature) (projectIDE-get-cache-exclude signature))
             (equal (projectIDE-get-project-whitelist signature) (projectIDE-get-cache-whitelist signature)))
        nil
      t)))

(defun projectIDE-update-project-config (signature &optional ErrorMessage caller)
  "Update specific project config in projectIDE-runtime-cache.
The project updated is specified by SIGNATURE.
ERRORMESSAGE indicates whether message is displayed to minibuffer
if there is any error.  Error message is not displayed by default.
This function return t if the project config is updated successfully.

CALLER is the function list calling this function.
It is uesed for debugging purpose.

Return
Type:\t\t bool
Descrip.: Return t if config is updated successfully.  Otherwise, return nil.

SIGNATURE
Type:\t\t string
Descrip.:\t String of number of signature.

ERRORMESSAGE
Type:\t\t bool
Descrip.: Display error message to minibuffer if it is t.

CALLER
Type:\t\t symbol list
Descrip.:\t Function list calling this function for debug purpose."
  
  (catch 'Error
    (let* ((project-config-file (projectIDE-get-config-file-path signature))
           (project (projectIDE-parse-config project-config-file nil
                                             (and projectIDE-debug-mode
                                                  (nconc (list 'projectIDE-update-project-config) caller)))))
        (unless project
          (projectIDE-message-handle 'Error
                                     (format "Update project config failed. Error reading %s." project-config-file)
                                     ErrorMessage
                                     'projectIDE-update-project-config
                                     (and projectIDE-debug-mode
                                        (nconc (list 'projectIDE-update-project-config) caller)))
          (throw 'Error nil))

        (unless (equal (projectIDE-project-name project) (projectIDE-get-project-name signature))
          (projectIDE-set-project-name (projectIDE-project-name project) signature)
          (fout<<projectIDE PROJECTIDE-RECORD-FILE 'projectIDE-runtime-record
                            (and projectIDE-debug-mode
                                 (nconc (list 'projectIDE-update-project-config) caller))))
        
        (projectIDE-set-cache-project signature project))

  (when projectIDE-debug-mode
    (projectIDE-message-handle 'Info
                               (format "Project config for project %s update successfully."
                                       (projectIDE-record-name (gethash signature projectIDE-runtime-record)))
                               nil
                               'projectIDE-update-project-config
                               (nconc (list 'projectIDE-update-project-config) caller)))
  t))

(defun projectIDE-update-cache-backend (signature &optional caller)
  "Perform a complete update on cache given by SIGNATURE.

CALLER is the function list calling this function.
It is uesed for debugging purpose.

SIGNATURE
Type:\t\t string
Descrip.:\t A project based unique ID.

CALLER
Type:\t\t symbol list
Descrip.:\t Function list calling this function for debug purpose."

  (catch 'Error
    ;; Test whether signature is in projectIDE-runtime-cache
    (unless (projectIDE-get-cache signature)
      (projectIDE-message-handle 'Error
                                 (format "Attempt to update cache %s not in projectIDE-runtime-cache." signature)
                                 nil
                                 'projectIDE-update-cache-backend
                                 (and projectIDE-debug-mode
                                      (nconc (list 'projectIDE-update-cache-backend) caller)))
      (throw 'Error nil))

    ;; Test whether config file need to be update and is able to update
    (when (projectIDE-config-need-update? signature
                                          (and projectIDE-debug-mode
                                               (nconc (list 'projectIDE-update-cache-backend) caller)))
      (unless (projectIDE-update-project-config signature nil
                                                (and projectIDE-debug-mode
                                                     (nconc (list 'projectIDE-update-cache-backend) caller)))
        (projectIDE-message-handle 'Error
                                   (format "Project config file for project %s not found." signature)
                                   nil
                                   'projectIDE-update-cache-backend
                                   (and projectIDE-debug-mode
                                        (nconc (list 'projectIDE-update-cache-backend) caller)))
        (throw 'Error nil)))

    (when (projectIDE-filter-changed? signature (and projectIDE-debug-mode
                                                     (nconc (list 'projectIDE-update-cache-backend) caller)))
      (projectIDE-set-cache-filter signature)
      (projectIDE-set-file-cache signature)
      (projectIDE-unset-file-cache-state signature))
    
    (fdex-update (projectIDE-get-file-cache signature))
    (projectIDE-set-file-cache-state signature)
    t))

(defun projectIDE-update-cache ()
  "An interactive function to update project cache of current buffer.
In simple term, it updates folders and files of the project."

  (interactive)
  (catch 'Error
    (unless (or projectIDE-p (projectIDE-initialize))
      (projectIDE-message-handle 'Error
                                 "projectIDE not initialized."
                                 t
                                 'projectIDE-update-cache)
      (throw 'Error nil))
    
    (let ((signature (projectIDE-get-Btrace-signature)))
      (unless signature
        (projectIDE-message-handle 'Warning
                                   "Current buffer not in project record."
                                   t
                                   'projectIDE-update-cache)
        (throw 'Error nil))

      (projectIDE-update-cache-backend signature (and projectIDE-debug-mode (list 'projectIDE-update-cache))))
      
    (when projectIDE-debug-mode
      (projectIDE-message-handle 'Info
                                 (format "Updated project cache for project %s" signature)
                                 nil
                                 'projectIDE-update-cache
                                 (nconc (list 'projectIDE-update-cache) caller)))
    t))

(defun projectIDE-track-buffer (signature &optional buffer caller)
  "Track BUFFER as a member of project given by SIGNATURE.
Tracking means
1) put buffer in projectIDE-runtime-BTrace
2) add buffer filename to opened buffer in projectIDE-runtime-record.

If buffer is not provide, current buffer is used.

CALLER is the function list calling this function.
It is uesed for debugging purpose.

SIGNATURE
Type:\t\t string
Descrip.:\t A project based unique ID.

BUFFER
Type\t\t: buffer
Descrip.:\t The buffer being identified.
\t\t\t If buffer is not provide, current buffer is used.

CALLER
Type:\t\t symbol list
Descrip.:\t Function list calling this function for debug purpose."
  
  (projectIDE-push-Btrace signature buffer)
  
  (unless (projectIDE-get-cache signature)
    (let (cache)
      (fin>>projectIDE (concat PROJECTIDE-CACHE-PATH signature) 'cache
                       (and projectIDE-debug-mode (nconc (list 'projectIDE-identify-project) caller)))
      (projectIDE-push-cache signature cache)
      (when (projectIDE-get-opened-buffer signature)
        (projectIDE-clear-opened-buffer signature))))
  
  (projectIDE-add-opened-buffer signature (buffer-file-name buffer))

  (projectIDE-set-project-last-open signature)

  (if (projectIDE-open-project-update-cache? signature)
      (projectIDE-update-cache-backend signature (and projectIDE-debug-mode (nconc (list 'projectIDE-identify-project) caller)))))

(defun projectIDE-untrack-buffer (&optional buffer caller)
"Untrack BUFFER as from project given by SIGNATURE.
It is designed to add to `kill-buffer-hook' as well.
Untrack means
1) remove buffer from projectIDE-runtime-BTrace
2) remove buffer filename from opened buffer in projectIDE-runtime-record.
If buffer is not provide, current buffer is used.


CALLER is the function list calling this function.
It is uesed for debugging purpose.

SIGNATURE
Type:\t\t string
Descrip.:\t A project based unique ID.

BUFFER
Type\t\t: buffer
Descrip.:\t The buffer being identified.
\t\t\t If buffer is not provide, current buffer is used.

CALLER
Type:\t\t symbol list
Descrip.:\t Function list calling this function for debug purpose."
  
  (let ((signature (projectIDE-get-Btrace-signature buffer)))
    (projectIDE-pop-Btrace buffer)
    (when (projectIDE-get-cache signature)
      (if (> (length (projectIDE-get-opened-buffer signature)) 1)
          (projectIDE-remove-opened-buffer signature (buffer-file-name buffer))
        (let ((cache (projectIDE-get-cache signature)))
          (fout<<projectIDE (concat PROJECTIDE-CACHE-PATH signature) 'cache
                            (and projectIDE-debug-mode (nconc (list 'projectIDE-untrack-buffer) (or caller (list 'kill-buffer-hook))))))
        (projectIDE-pop-cache signature)))))

(defun projectIDE-before-save-new-file ()
  "This function is designed to add to `before-save-hook'.
It detects whether the newly created file is a member of a project.
It also test whether the project enabled background caching.

If yes, it will flag the file to be priority caching
at `projectIDE-after-save-new-file'."

  (let ((signature (projectIDE-get-signature-by-path (buffer-file-name))))
    (when (and signature
               (not (file-exists-p (buffer-file-name)))
               (projectIDE-background-update-cache? signature))
      (setq projectIDE-save-cache (list signature (buffer-file-name))))))

(defun projectIDE-after-save-new-file ()
  "This function is designed to add to `after-save-hook'.
It flags the newly created file to cache at priority."

  (when (and projectIDE-save-cache)
    (when (equal (cdr projectIDE-save-cache) (buffer-file-name))
      (projectIDE-track-buffer (car projectIDE-save-cache) (cdr projectIDE-save-cache))
      (fdex-add-priority-update-path (projectIDE-get-file-cache (car projectIDE-save-cache))
                                     (file-name-directory (buffer-file-name))))
    (setq projectIDE-save-cache nil)))

(defun projectIDE-before-emacs-kill ()
  "Write all cache in projectIDE-runtime-cache to harddisk.
This function is designed to add to `kill-emacs-hook'."

  (remove-hook 'kill-buffer-hook 'projectIDE-untrack-buffer)
  
  (let ((signatures (projectIDE-get-all-caching-signature)))
    (dolist (signature signatures)
      (let ((cache (projectIDE-get-cache signatue)))
        (fout<<projectIDE (concat PROJECTIDE-CACHE-PATH signature) 'cache (list 'projectIDE-before-emacs-kill 'kill-emacs-hook)))
      (projectIDE-pop-cache signature))))

(defun projectIDE-generate-association-list (buffer)
  "Return a list of files which have same filename as BUFFER.

Return
Type\t\t: list of string
Descrip.:\t A list of file paths having same filename as BUFFER.

BUFFER
Type\t\t: buffer
Descrip.:\t The buffer being identified."
  
  (let* ((signature (projectIDE-get-Btrace-signature buffer))
         (filename (buffer-file-name buffer))
         regexp)

    (setq regexp (concat (file-name-as-directory ".*\\")
                         (file-name-sans-extension (file-name-nondirectory filename))
                         (file-name-as-directory "[^\\")
                         "]*\\.+.*$"))
    
    (projectIDE-get-file-list signature t (lambda (test) (string-match regexp test)))))

;; Caching function ends ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



















;;; Fetching data function ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;fff (defun projectIDE-prompt (prompt choices &optional initial-input)
;;fff (defun projectIDE-get-project-list ())
;;fff (defun projectIDE-open-project (&optional caller)) - I
;;fff (defun projectIDE-get-file-list (signature &optional full filter caller))
;;fff (defun projectIDE-open-file (prefix)) - I

(defun projectIDE-prompt (prompt choices &optional initial-input)
  "Create a PROMPT to choose from CHOICES which is a list.
Return the selected result.

Return
Type:\t\t type of the CHOICES list
Descrip.:\t Return the user choice.

PROMPT
Type:\t\t string
Descrip.: Prompt message.

CHOICES
Type:\t\t list of any type
Descrip.: A list of choices to let user choose.

INITIAL-INPUT
Type:\t\t string
Descrip.:\t Initial input for the prompt."

  (cond
     ;; ido
     ((eq projectIDE-completion-system 'ido)
      (ido-completing-read prompt choices nil nil initial-input))

     ;; helm
     ((eq projectIDE-completion-system 'helm)
      (if (fboundp 'helm-comp-read)
          (helm-comp-read prompt choices
                          :initial-input initial-input
                          :candidates-in-buffer t
                          :must-match 'confirm)
        (projectIDE-message-handle 'Warning
                                   "Problem implementing helm completion. Please check `projectIDE-completion-system'.
projectIDE will use default completion instead."
                                   t
                                   'projectIDE-prompt)
        (completing-read prompt choices nil nil initial-input)))

     ;; grizzl
     ((eq projectIDE-completion-system 'grizzl)
      (if (and (fboundp 'grizzl-completing-read)
               (fboundp 'grizzl-make-index))
          (grizzl-completing-read prompt (grizzl-make-index choices))
        (projectIDE-message-handle 'Warning
                                   "Problem implementing grizzl completion. Please check `projectIDE-completion-system'.
projectIDE will use default completion instead."
                                   t
                                   'projectIDE-prompt)
        (completing-read prompt choices nil nil initial-input)))

     ;; ivy
     ((eq projectIDE-completion-system 'ivy)
      (if (fboundp 'ivy-completing-read)
          (ivy-completing-read prompt choices nil nil initial-input)
        (projectIDE-message-handle 'Warning
                                   "Problem implementing ivy completion. Please check `projectIDE-completion-system'.
projectIDE will use default completion instead."
                                   t
                                   'projectIDE-prompt)
        (completing-read prompt choices nil nil initial-input)))

     ;; default
     (t (completing-read prompt choices nil nil initial-input))))

(defun projectIDE-get-project-list ()
  "Return a list of three lists from projectIDE-runtime-record.

The first list is a list of project names.
The second list is a list of project root paths.
The third list is a list of project signatures.
The nth item in these lists are pointing to same project.

Return
Type:\t\t list of three lists
Descip.:\t Relative items in each list
\t\t\t represent a record in projectIDE-runtime-record.
Example:\t ((\"foo\" \"bar\")
\t\t\t (\"path-of-foo\" \"path-of-bar\")
\t\t\t (\"signature-of-foo\" \"signature-of-bar\"))"
  
  (let ((signatures (projectIDE-get-all-singatures))
        paths
        names)

      (setq signatures (sort signatures (lambda (record1 record2) (time-less-p (projectIDE-get-project-last-open record1)
                                                                               (projectIDE-get-project-last-open record2)))))

      (dolist (signature signatures)
        (setq paths (nconc paths (list (projectIDE-get-project-path signature))))
        (setq names (nconc names (list (projectIDE-get-project-name signature)))))

      ;; Solving duplicate project name
      (let ((max (1- (or (length names) 1)))
            (i 0)
            j
            count)
        (while (< i max)
          (setq j (1+ i))
          (setq count 1)
          (let ((test (nth i names))
                modified)
            (while (<= j max)
              (when (equal test (nth j names))
                (setf (nth j names) (concat (nth j names) "-" (int-to-string (setq count (1+ count)))))
                (setq modified t))
              (setq j (1+ j)))
            (when modified
              (setf (nth i names) (concat (nth i names) "-1"))))
          (setq i (1+ i))))
      
      (list names paths signatures)))

(defun projectIDE-open-project (&optional caller)
  "Open certain project with user prompt.

CALLER is the function list calling this function.
It is uesed for debugging purpose.

CALLER
Type:\t\t symbol list
Descrip.:\t Function list calling this function for debug purpose."

  (interactive)
  (catch 'Error
    (unless (or projectIDE-p (projectIDE-initialize))
      (projectIDE-message-handle 'Error
                                 "projectIDE not initialized."
                                 t
                                 'projectIDE-open-project
                                 (nconc (list 'projectIDE-open-project) caller))
      (throw 'Error nil))

        
    (let* ((list (projectIDE-get-project-list))
           (names (nth 0 list))
           (paths (nth 1 list))
           (signatures (nth 2 list))
           choice
           signature
           projectRoot
           project
           cache
           opened-buffer)

      (cond
       ((eq projectIDE-open-project-prompt-type 'path)
        (setq choice (projectIDE-prompt "Choose project: " paths))
        (setq signature (nth (position choice paths :test 'equal) signatures)))
       (t
        (setq choice (projectIDE-prompt "Choose project: " names))
        (setq signature (nth (position choice names :test 'equal) signatures))))

      (setq projectRoot (projectIDE-get-project-path signature))
      
      ;; Check whether project has been opened
      ;; If yes, just open the last opened buffer
      (when (projectIDE-get-cache signature)
        (projectIDE-message-handle 'Info
                                   (format "Project [%s] had been opened already." (projectIDE-get-project-name signature))
                                   t
                                   'projectIDE-open-project
                                   (and projectIDE-debug-mode (nconc (list 'projectIDE-open-project) caller)))
        (find-file (car (projectIDE-get-opened-buffer signature)))
        (throw 'Error nil))

      ;; Check whether .projectIDE exists under path
      (unless (file-exists-p (concat projectRoot PROJECTIDE-PROJECTROOT-IDENTIFIER))
        (projectIDE-message-handle 'Warning
                                   (format ".projectIDE file not exists under %s.
If you moved the project, you can use `projectIDE-index-project' to reindex it." projectRoot)
                                   t
                                   'projectIDE-open-project
                                   (and projectIDE-debug-mode (nconc (list 'projectIDE-open-project) caller)))
        (throw 'Error nil))

      
      ;; Check whether .projectIDE is able to be parsed
      (unless (setq project (projectIDE-parse-config (concat projectRoot PROJECTIDE-PROJECTROOT-IDENTIFIER)))
        (projectIDE-message-handle 'Error
                                   (format "Open project terminated due to .projectIDE file under %s corrupted." projectRoot)
                                   t
                                   'projectIDE-open-project
                                   (and projectIDE-debug-mode (nconc (list 'projectIDE-open-project) caller)))
        (throw 'Error nil))

      ;; check if there is cache file
      (unless (file-exists-p (concat PROJECTIDE-CACHE-PATH signature))
          (projectIDE-cache-create projectRoot))
      
      ;; check if cache load successfully
      (unless (fin>>projectIDE (concat PROJECTIDE-CACHE-PATH signature) 'cache (and projectIDE-debug-mode
                                                                                    (nconc (list 'projectIDE-open-project) caller)))
        (projectIDE-message-handle 'Error
                                   (format "Unable to load project cache: %s %s" (projectIDE-get-project-name signature) signature)
                                   t
                                   'projectIDE-open-project
                                   (and projectIDE-debug-mode (nconc (list 'projectIDE-open-project) caller)))
        (throw 'Error nil))
      
      (setq opened-buffer (nreverse (copy-tree (projectIDE-cache-opened-buffer cache))))

      (if (and opened-buffer projectIDE-open-last-opened-files)
          (dolist (file opened-buffer)
            (when (file-readable-p file)
              (find-file file)))
        (dired projectRoot)))))

(defun projectIDE-get-file-list (signature &optional full filter caller)
  "Get a file list of project given by SIGNATURE.

If FULL is non-nil, the file list contains full paths,
otherwise contains paths relative to project root.

FILTER is a predicate funtion accepting one argument
to test each of the entry.

CALLER is the function list calling this function.
It is uesed for debugging purpose.

Return
Type:\t\t list of string
Descrip.:\t A list of file of given project.

SIGNATURE
Type:\t\t string
Descrip.:\t A project based unique ID.

FULL
Typee:\t\t bool
Descrip.:\t Return path relative to project root if nil.
\t\t\t Otherwise return full path.

FILTER
Type:\t\t a predicate funtion
Descrip.:\t A predicate function taking one string argument.
\t\t\t The predicate function should return t if the argument is accepted.
Example:\t (projectIDE-get-file-list
\t\t\t    (\"signature\" t (lambda (test) (if (string-match \"*.cpp\" test) nil t))))

CALLER
Type:\t\t symbol list
Descrip.:\t Function list calling this function for debug purpose."
  
  (when (projectIDE-pre-prompt-update-cache? signature)
    (projectIDE-update-cache-backend signature (and projectIDE-debug-mode (nconc (list 'projectIDE-get-file-list) caller))))
  (if filter
      (let ((filelist (fdex-get-filelist (projectIDE-get-file-cache signature) full))
            filelist-1)
        (dolist (file filelist)
          (when (funcall filter file)
            (setq filelist-1 (nconc filelist-1 (list file)))))
        filelist-1)
    (fdex-get-filelist (projectIDE-get-file-cache signature) full)))

(defun projectIDE-open-file (prefix)
  "An interative function to prompt a file for opening.

The file list prompted contains files from project of current buffer.
Unless current buffer is not a recorded project,
it will project for all files form opened projects instesd.

When PREFIX is provided, the file list contains files from all opened projects."
  
  (interactive "p")
  (catch 'Error

    (unless (or projectIDE-p (projectIDE-initialize))
      (projectIDE-message-handle 'Error
                                 "projectIDE not initialized."
                                 t
                                 'projectIDE-open-file)
      (throw 'Error nil))
    
    (let (sources
           prompt
           project-prefix
           choice)
      
      ;; When prefixed, open file from all current opened project.
      (if (eq prefix 1)
          (progn
            (setq sources (list (projectIDE-get-Btrace-signature (current-buffer))))
            (unless (car sources)
              (setq sources (projectIDE-get-all-caching-signature))))
        (setq sources (projectIDE-get-all-caching-signature)))

      (unless (car sources)
        (projectIDE-message-handle 'Warning
                                   "Current buffer is not an indexed project."
                                   t
                                   'projectIDE-open-file)
        (throw 'Error nil))

      ;; Determined if project prefix should be used
      (when (and projectIDE-use-project-prefix (= (length sources) 1))
        (setq project-prefix
              (concat (file-name-as-directory (concat "[" (projectIDE-get-project-name (car sources)) "] ")) " ")))

      ;; Create promt-file-list from different source
      (dolist (source sources)
        (if project-prefix
            (progn
              (setq prompt (projectIDE-get-file-list source nil nil (and projectIDE-debug-mode (list 'projectIDE-open-file))))
              (setq prompt (mapcar (apply-partially 'concat project-prefix) prompt)))
          (setq prompt (nconc prompt (projectIDE-get-file-list source t nil (and projectIDE-debug-mode (list 'projectIDE-open-file)))))))

      (setq choice (projectIDE-prompt "Open file: " prompt))
      
      (when project-prefix
        (setq choice 
              (concat
               (projectIDE-get-project-path (car sources))
               (string-remove-prefix project-prefix choice))))
      
      (unless (file-exists-p choice)
        (projectIDE-message-handle 'Warning
                                   (format "%s no longer exist.\nYou may need to call `projectIDE-update-cache' first." choice)
                                   t
                                   'projectIDE-open-file)
        (throw 'Error nil))

      (find-file choice))))

;;; Fetching data function ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




















(cl-defmacro projectIDE-create (projectType &key templateDir defaultDir documentation)
  "Create projectIDE-create-X function."
  (let* ((projectType (symbol-name projectType))
         (templateDir (file-name-as-directory templateDir))
         (defaultDir (file-name-as-directory defaultDir))
         (documentation (or documentation (format
                                           "Create new '%s' project.\nProjectIDE will create a new folder named PROJECTNAME under DIR"
                                           projectType))))
    
    ;; Check if the template directory and default directory exist
    (if (and (file-directory-p templateDir)
             (or (file-directory-p defaultDir)
                 (setq defaultDir (or projectIDE-create-defaultDir user-emacs-directory))))
        
        ;; Function template
        `(defun ,(intern (concat "projectIDE-create-" (downcase projectType))) (projectName dir)

           ;; Documentation
           ,documentation

           ;; Interactive call to ask for project name and project directory
           (interactive (list (read-string "Project Name: ")
                              (read-directory-name "Create project at: " ,defaultDir)))
           (setq dir (file-name-as-directory (expand-file-name dir)))

           (catch 'Error
             ;; Validate input
             ;; Check for initialization
             (unless (or projectIDE-p (projectIDE-initialize))
               (projectIDE-message-handle 'Error
                                          "projectIDE is not initialized."
                                          nil
                                          'projectIDE-create)
               (throw 'Error nil))
             ;; Prevent null string project name
             (when (string= projectName "")
               (projectIDE-message-handle 'Error
                                          "Project name cannot be empty string."
                                          t
                                          'projectIDE-create)
               (throw 'Error nil))
             ;; Make sure project root directory can be generated
             (when (file-exists-p (concat dir projectName))
               (projectIDE-message-handle 'Error
                                          (format "Folder \"%s\" already exists in \"%s\". Operation cancelled." projectName dir)
                                          t
                                          'projectIDE-create)
               (throw 'Error nil))

             ;; Ask for user prompt
             (unless (or (not projectIDE-create-require-confirm) ;; Confirm project creation guard
                         (y-or-n-p (projectIDE-message-handle
                                    'Info
                                    (format "\nProject\t\t\t\t: %s\nTemplate\t\t\t: %s\nProject Directory\t: %s\nCreate Project ? "
                                            projectName ,templateDir (concat dir projectName))
                                    nil
                                    'projectIDE-create)))
               (projectIDE-message-handle 'Info
                                          "Projection creation cancelled."
                                          t
                                          'projectIDE-create)
               (throw 'Error nil))


             ;; Project create here
             (let* ((projectRoot (file-name-as-directory (concat dir projectName)))
                    (projectConfig (concat projectRoot PROJECTIDE-PROJECTROOT-IDENTIFIER)))
               ;; Create project structure by template
               (make-directory projectRoot t)
               (unless (file-writable-p projectRoot)
                 (projectIDE-message-handle 'Error
                                            (format "Project directory \"%s\" is not writable." dir)
                                            t
                                            'projectIDE-create)
                 (throw 'Error nil))
               (copy-directory ,templateDir projectRoot nil nil t)
               (projectIDE-new-project projectRoot (and projectIDE-debug-mode
                                                        (list 'projectIDE-create)))
               (run-hooks 'projectIDE-global-project-create-hook)
               (projectIDE-message-handle 'Info
                                          (format "Project Created\nProject\t\t\t\t: %s\nTemplate\t\t\t: %s\nProject Directory\t: %s"
                                                  projectName ,templateDir projectRoot)
                                          t
                                          'projectIDE-create))))
      
      ;; Macro error message
      (projectIDE-message-handle 'Error
                                 (format "Template directory \"%s\" error\nEither not exists, not directory or non-accessible." templateDir)
                                 t
                                 'projectIDE-create))))


(defun projectIDE-initialize ()
  "Initialize projectIDE."
  (interactive)
  (catch 'Error
    (when projectIDE-p
      (projectIDE-message-handle 'Warning
                                 "projectIDE has already initialized."
                                 nil
                                 'projectIDE-initialize)
      (throw 'Error nil))
    ;; Check whether projectIDE database folder exist
    (unless (file-exists-p projectIDE-database-path)
      (make-directory projectIDE-database-path))
    ;; Check global RECORD file exist
    (unless (file-exists-p PROJECTIDE-RECORD-FILE)
      (write-region "" nil PROJECTIDE-RECORD-FILE t 'inhibit))
    ;; Check cache folder exist
    (unless (file-exists-p PROJECTIDE-CACHE-PATH)
      (make-directory PROJECTIDE-CACHE-PATH))
    ;; Check log folder exist
    (unless (file-exists-p PROJECTIDE-LOG-PATH)
      (make-directory PROJECTIDE-LOG-PATH))

    (if (fin>>projectIDE PROJECTIDE-RECORD-FILE 'projectIDE-runtime-record)
        (progn
          (unless projectIDE-runtime-record
            (setq projectIDE-runtime-record (make-hash-table :test 'equal :size 40)))
          (setq projectIDE-runtime-cache (make-hash-table :test 'equal :size 20)
                projectIDE-runtime-Btrace (make-hash-table :test 'eql :size 40))
          (projectIDE-message-handle 'Info
                                     "projectIDE starts successfully."
                                     t
                                     'projectIDE-initialize)
          (add-hook 'find-file-hook 'projectIDE-identify-project)
          (add-hook 'kill-buffer-hook 'projectIDE-untrack-buffer)
          (add-hook 'kill-emacs-hook 'projectIDE-before-emacs-kill)
          (add-hook 'before-save-hook 'projectIDE-before-save-new-file)
          (add-hook 'after-save-hook 'projectIDE-after-save-new-file)
          (setq projectIDE-p t)
          (let ((buffers (buffer-list)))
            (dolist (buffer buffers)
              (projectIDE-identify-project buffer 'projectIDE-initialize)))
          (run-hooks 'projectIDE-initialize-hook))
      (projectIDE-message-handle 'Error
                                 (format "projectIDE starts fail. Unable to read record file at %s" PROJECTIDE-RECORD-FILE)
                                 t
                                 'projectIDE-initialize))
    ;; Return value
    projectIDE-p))

(defun projectIDE-terminate ()
  "Terminate projectIDE."
  (interactive)
  (when projectIDE-p
    (remove-hook 'find-file-hook 'projectIDE-identify-project)
    (remove-hook 'kill-buffer-hook 'projectIDE-untrack-buffer)
    (remove-hook 'kill-emacs-hook 'projectIDE-before-emacs-kill)
    (remove-hook 'before-save-hook 'projectIDE-before-save-new-file)
    (remove-hook 'after-save-hook 'projectIDE-after-save-new-file)
    (setq projectIDE-runtime-record nil
          projectIDE-runtime-cache nil
          projectIDE-runtime-Btrace nil
          projectIDE-p nil)
    (run-hooks 'projectIDE-terminate-hook)))

(provide 'projectIDE)
;;; projectIDE.el ends here
