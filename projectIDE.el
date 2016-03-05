;;; projectIDE.el --- project configuration file
;;
;; Copyright (C) 2015 Mola-T
;; Author: Mola-T <Mola@molamola.xyz>
;; URL: https://github.com/mola-T/projectIDE
;; Version: 0.1
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





;;; Config file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; (defun projectIDE-parse-config (file &optional errormessage caller))
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
        (while (search-forward-regexp projectIDE-default-config-key-regexp-string nil t)
          (save-excursion

            ;; Identify key
            (beginning-of-line)
            (let ((found nil)
                  (line-end (line-end-position))
                  (keylist projectIDE-default-config-key)
                  (counter 0))
              (while (and (not found) (car-safe keylist))
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
                        (setf (projectIDE-project-signature project) (trim-string (buffer-substring-no-properties (point) line-end))))
                       ((= counter 1) ;; "^name="
                        (when (projectIDE-project-name project)
                          (projectIDE-message-handle 'Error
                                                     (format "Config file corrupt. 'name' in %s definded more than once." file)
                                                     errormessage
                                                     'projectIDE-parse-config
                                                     (nconc (list 'projectIDE-parse-config) caller))
                          (throw 'parse-error nil))
                        (setf (projectIDE-project-name project) (trim-string (buffer-substring-no-properties (point) line-end))))
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
                       ((= counter 5) ;; "^cachemode="
                        (when (projectIDE-project-cachemode project)
                          (projectIDE-message-handle 'Error
                                                     (format "Config file corrupt. 'cachemode' in %s definded more than once." file)
                                                     errormessage
                                                     'projectIDE-parse-config
                                                     (nconc (list 'projectIDE-parse-config) caller))
                          (throw 'parse-error nil))
                        (setf (projectIDE-cachemode project) (string-to-int (trim-string (buffer-substring-no-properties (point) line-end)))))
                       ((= counter 6) ;; "^module="
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

;; (defun projectIDE-validate-config (&optional file errormessage caller)

;;   (interactive "p")
  
;;   )



;; Indexing ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; (defun projectIDE-generate-signature (&optional caller))
;; (defun projectIDE-new-project (path &optional caller))
;; (defun projectIDE-root-create (path &optional caller))
;; (defun projectIDE-record-create (configfile &optional caller))
;; (defun projectIDE-cache-create (configfile &optional caller))
;; (defun projectIDE-update-project-config (signature &optional ErrorMessage caller))
;; (defun projectIDE-manipulate-filter (projectRoot list))

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
          (projectIDE-record-create-date record) (current-time)
          (projectIDE-record-last-modify record) (current-time))
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
    (do-list (entry list)
      (projectIDE-add-to-list 'return
                              (concat projectRoot
                                      (replace-regexp-in-string "\\*" ".*"
                                                     (replace-regexp-in-string "\\." "\\\\." entry))
                                      "\\'")))
    return))


;;; Caching
(defun projectIDE-config-need-update? (signature &optional caller)
  "Return t if config file of given SIGNATURE need to be updated.
The config file needs to be updated iif its modification time is
later than the last config update time in cache.

Return
Type:\t\t bool
Descrip.:\t t if config file needs updat, otherwise nil.

SIGNATURE
Type:\t\t string
Descrip.:\t A project based unique ID.

CALLER
Type:\t\t symbol list
Descrip.:\t Function list calling this function for debug purpose."

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
      nil)))

(defun projectIDE-filter-changed? (signature &caller caller)

  (catch 'Error
    (unless (projectIDE-get-cache signature)
      (throw 'Error nil))
    
    (if (and (equal (projectIDE-get-exclude-from-project signature) (projectIDE-get-exclude-from-cache signature))
             (equal (projectIDE-get-whitelist-from-project signature) (projectIDE-get-whitelist-from-cache signature)))
        nil
      t)))

(defun projectIDE-update-project-config (signature &optional ErrorMessage caller)
  "Update specific project config in projectIDE-opened-project.
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
        (projectIDE-set-project-in-cache project signature))

  (when projectIDE-debug-mode
    (projectIDE-message-handle 'Info
                               (format "Project config for project %s update successfully."
                                       (projectIDE-record-name (gethash signature projectIDE-runtime-record)))
                               nil
                               'projectIDE-update-project-config
                               (nconc (list 'projectIDE-update-project-config) caller)))
  t))

(defun projectIDE-update-cache-backend (signature &optional caller)
  "Full update "

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

          
    

    
    (fdex-update (projectIDE-cache-file-cache cache)))
  t)






(defun projectIDE-update-cache (&optional caller)
  "An interactive function to update project cache of current buffer.
In simple term, it updates folders and files of the project.

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
                                 'projectIDE-update-cache
                                 (nconc (list 'projectIDE-update-cache) caller))
      (throw 'Error nil))
    
    (let ((signature (gethash (current-buffer) projectIDE-buffer-trace)))
      (if signature
          
        (projectIDE-message-handle 'Warning
                                   "Current buffer not in project record."
                                   t
                                   'projectIDE-update-cache
                                   (and projectIDE-debug-mode
                                        (nconc (list 'projectIDE-update-cache) caller)))))
    (when projectIDE-debug-mode
      (projectIDE-message-handle 'Info
                                 (format "User invoked update project cache for %s" (or (buffer-file-name) "invalid buffer"))
                                 nil
                                 'projectIDE-update-cache
                                 (nconc (list 'projectIDE-update-cache) caller)))))



(defun projectIDE-before-save-new-file ()
  "This function is designed to add to before-save-hook.
It detects newly created file and will update projectIDE-runtime-cache
at projectIDE-after-save-new-file if the file is a member of a project."
  (when (and (not (file-exists-p (buffer-file-name)))
             (projectIDE-get-signature-by-path (buffer-file-name)))
    (setq projectIDE-save-cache (buffer-file-name))))

(defun projectIDE-after-save-new-file ()
  "This function is designed to add to after-save-hook.
It added the saved file to project cache if
it is newly created and a member of project."
  (when (and projectIDE-save-cache
             (equal projectIDE-save-cache (buffer-file-name)))
    (catch 'Error
      (let ((signature (projectIDE-get-signature-by-path projectIDE-save-cache)))

        ;; If project is not currently it cache, load it to cache
        (if (gethash signature projectIDE-opened-project)
            (let )

          (let ((project (projectIDE-parse-config (projectIDE-record-path (gethash signature projectIDE-runtime-record))))
                (cache nil))

            ;; Check if project object successfully loaded
            (unless (and project (projectIDE-project-signature project))
              (throw 'Error nil))
            ;; Check if cache file load successfully
            (unless (fin>>projectIDE (concat PROJECTIDE-CACHE-PATH signature) 'cache)
              (setq cache (make-projectIDE-cache :project project)))

            (puthash signature project projectIDE-opened-project)
            (puthash signature cache projectIDE-runtime-cache)
            
          )
        ))) 
  (setq projectIDE-save-cache nil)))

(defun projectIDE-identify-project (&optional buffer caller)
  "This function check whether BUFFER is a indexed project.
If it is a indexed project, it ensures
1) the project is under projectIDE-runtime-cache
2) it is under opened buffer in the project cache
If BUFFER is not provided, current buffer is used instead.

CALLER is the function list calling this function.
It is uesed for debugging purpose.

BUFFER
Type\t\t: buffer
Descrip.:\t The buffer being identified.

CALLER
Type:\t\t symbol list
Descrip.:\t Function list calling this function for debug purpose."
  
  (let ((found nil)
        (signature nil))

    (let ((opened-project-signatures (hash-table-keys projectIDE-runtime-cache)))
      ;; Find project in runtime cache
      ;; If found set found to t
      (while (and (not found) (buffer-file-name buffer) (car-safe opened-project-signatures))
        (when (string-prefix-p
               (projectIDE-record-path
                (gethash (car opened-project-signatures) projectIDE-runtime-record)) ;; get RECORD from current opened project
               (buffer-file-name buffer))
          (setq signature (car opened-project-signatures))
          (setq found t))
        (setq opened-project-signatures (cdr opened-project-signatures))))

    ;; Search in project RECORD
    ;; If found set found to t
    ;; And add project to projectIDE-opened-project
    (unless found
      (when (buffer-file-name buffer)
        (setq signature (projectIDE-get-signature-by-path (buffer-file-name buffer)
                                                        (and projectIDE-debug-mode
                                                          (nconc (list 'projectIDE-identify-project) (and caller (list 'find-file-hook)))))))
      (when signature
        (setq found t)))
    
    ;; Search .projectIDE up directory
    ;; Only apply to call where buffer is not provided
    ;; If found set found to t
    (unless (or buffer found)
      (let ((search-countdown projectIDE-config-file-search-up-level)
            (path (file-name-directory (buffer-file-name))))
        (while (and (not found) (> search-countdown 0))
          (when (file-exists-p (concat path PROJECTIDE-PROJECTROOT-IDENTIFIER))
            (let* ((projectRoot (concat path PROJECTIDE-PROJECTROOT-IDENTIFIER))
                   (project (projectIDE-parse-config projectRoot nil
                                                     (and projectIDE-debug-mode
                                                          (nconc (list 'projectIDE-identify-project) (and caller (list 'find-file-hook)))))))
              (if (projectIDE-project-signature project)
                  ;; found .projectIDE with signature
                  (progn
                    (setq signature (projectIDE-project-signature project))
                    (projectIDE-record-create projectRoot
                                              (and projectIDE-debug-mode
                                                   (nconc (list 'projectIDE-identify-project) (and caller (list 'find-file-hook)))))
                    (unless (file-exists-p (concat PROJECTIDE-CACHE-PATH signature))
                      (projectIDE-cache-create projectRoot
                                               (and projectIDE-debug-mode
                                                    (nconc (list 'projectIDE-identify-project) caller))))
                    (setq found t))
                ;; found .projectIDE without signature
                (if (y-or-n-p
                     (projectIDE-message-handle
                      'Info
                      (format ".projectIDE root file found at %s.\nMake this path as project root and index the project? " path)
                      nil
                      'projectIDE-identify-project
                      (and projectIDE-debug-mode
                           (nconc (list 'projectIDE-identify-project) caller))))
                    (progn
                      (projectIDE-new-project path (and projectIDE-debug-mode
                                                        (nconc (list 'projectIDE-identify-project) caller)))
                      (setq signature (projectIDE-get-signature-by-path (buffer-file-name buffer)))
                      (setq found t)
                      (projectIDE-message-handle
                       'Info
                       (format "Project Indexed\nProject\t\t\t\t: %s\nProject Directory\t: %s"
                               (file-name-nondirectory (directory-file-name (file-name-directory projectRoot))) path)
                       t
                       'projectIDE-identify-project
                       (and projectIDE-debug-mode
                            (nconc (list 'projectIDE-identify-project) caller))))
                  (projectIDE-message-handle 'Info
                                             "File opened without indexing."
                                             t
                                             'projectIDE-identify-project
                                             (and projectIDE-debug-mode
                                                  (nconc (list 'projectIDE-identify-project) caller)))
                  (setq search-countdown -1)))))
          
          (setq search-countdown (1- search-countdown))
          (setq path (file-name-directory (directory-file-name path))))))

        
        (when found
          (let ((project (projectIDE-parse-config
                          (concat (projectIDE-record-path (gethash signature projectIDE-runtime-record)) PROJECTIDE-PROJECTROOT-IDENTIFIER)
                          (and projectIDE-debug-mode
                               (nconc (list 'projectIDE-identify-project) caller))))
                (cache nil)
                (flag-update-cache nil))

            ;; Add to opened project if it is not there yet
            (unless (gethash signature projectIDE-opened-project)
              (puthash signature project projectIDE-opened-project)
              (and projectIDE-open-file-update-cache (setq flag-update-cache t)))

            ;; Create project cache if it is not ther yet
            (unless (gethash signature projectIDE-runtime-cache)
              (fin>>projectIDE (concat PROJECTIDE-CACHE-PATH signature) 'cache (and projectIDE-debug-mode
                                                                                    (nconc (list 'projectIDE-identify-project) caller)))
              (puthash signature cache projectIDE-runtime-cache))


            (setq project (gethash signature projectIDE-opened-project))
            (setq cache (gethash signature projectIDE-runtime-cache))
          
            ;; (let ((project (gethash signature projectIDE-opened-project))
            ;;       (cache (gethash signature projectIDE-runtime-cache)))
            ;; Add to opened buffer in project cache
            (setf (projectIDE-cache-opened-buffer cache) (projectIDE-add-to-list (projectIDE-cache-opened-buffer cache) (buffer-file-name buffer)))
            ;; Add to buffer-trace
            (puthash (or buffer (current-buffer)) signature projectIDE-buffer-trace)

            ;; Update project cache
            (when flag-update-cache
              (projectIDE-update-cache 'projectIDE-identify-project)))
          
          ;; Change last modify time in record
          (setf (projectIDE-record-last-modify (gethash signature projectIDE-runtime-record))
                (time-to-days (current-time)))
          
          (unless buffer
            (projectIDE-message-handle
             'Info
             (format "Opened file from project [%s]" (projectIDE-project-name (gethash signature projectIDE-opened-project)))
             t
             'projectIDE-identify-project
             (and projectIDE-debug-mode
                  (nconc (list 'projectIDE-identify-project) caller)))))

        (when (and projectIDE-debug-mode (not found))
          (projectIDE-message-handle
           'Info
           (format "Opened buffer '%s' is not a indexed project." (or (buffer-file-name) "invalid buffer"))
           nil
           'projectIDE-identify-project
           (and projectIDE-debug-mode
                (nconc (list 'projectIDE-identify-project) caller))))))

(defun projectIDE-kill-project-buffer ()
  "Remove opened-buffer from cache when killing a project buffer.
This function should be added to `kill-buffer-hook'"
  (when (gethash (current-buffer) projectIDE-buffer-trace)
    (let* ((currentBuffer (current-buffer))
           (signature (gethash currentBuffer projectIDE-buffer-trace))
           (cache (gethash signature projectIDE-runtime-cache))
           (opened-buffer (and cache (projectIDE-cache-opened-buffer cache))))
      (remhash currentBuffer projectIDE-buffer-trace)
      (if (> (length opened-buffer) 1)
          (setf (projectIDE-cache-opened-buffer cache) (remove (buffer-file-name currentBuffer) opened-buffer))
        (fout<<projectIDE (concat PROJECTIDE-CACHE-PATH signature) 'cache (and projectIDE-debug-mode
                                                                               (list 'projectIDE-identify-project)))
        (remhash signature projectIDE-opened-project)
        (remhash signature projectIDE-runtime-cache)))))


(defun projectIDE-open-file (prefix)
  "An interative function to prompt a file for opening.
The file list contains files from project of current buffer.
When PREFIX is provided, the file list contains files from all opened projects."
  (interactive "p")
  (catch 'Error

    (unless (or projectIDE-p (projectIDE-initialize))
      (projectIDE-message-handle 'Error
                                 "projectIDE not initialized."
                                 t
                                 'projectIDE-open-file)
      (throw 'Error nil))
    
    (let ((source-signature (list (gethash (current-buffer) projectIDE-buffer-trace)))
          (prompt-file-list nil)
          (project-prefix nil)
          (projectRoot nil))

      ;; When prefixed, open file from all current opened project.
      (unless (= prefix 1)
        (setq source-signature (hash-table-keys projectIDE-opened-project)))

      (unless (car-safe source-signature)
        (projectIDE-message-handle 'Warning
                                   "Current buffer is not an indexed project."
                                   t
                                   'projectIDE-open-file)
        (throw 'Error nil))

      ;; Determined if project prefix should be used
      (when (and projectIDE-use-project-prefix (= (length source-signature) 1))
        (setq project-prefix
              (concat
               "["
               (projectIDE-project-name (gethash (car source-signature) projectIDE-opened-project))
               "]")))

      ;; Create promt-file-list from different source
      (while (car-safe source-signature)
        (setq projectRoot (projectIDE-record-path (gethash (car source-signature) projectIDE-runtime-record)))
        (dolist (element (projectIDE-get-project-file-list (car source-signature)))
          (setq prompt-file-list
                (nconc prompt-file-list
                       (list (concat (file-name-as-directory (or project-prefix projectRoot)) element)))))
        (setq source-signature (cdr source-signature)))

      
      (let ((path (projectIDE-prompt "Open file: " prompt-file-list)))
        (when project-prefix
          (string-match (file-name-as-directory project-prefix) path)
          (setq path (substring path (match-end 0) nil)))

        (unless (file-exists-p path)
          (projectIDE-message-handle 'Warning
                                     (format "%s no longer exist.\nYou may need to call projectIDE-update-cache first." path)
                                     t
                                     'projectIDE-open-file)
          (throw 'Error nil))

        (find-file path)))))

(defun projectIDE-get-project-list ()
  "Return a list of project from projectIDE-runtime-record."
  (let ((record-list (hash-table-values projectIDE-runtime-record))
        (project-name-list))
    (sort record-list
          (lambda (record1 record2) (> (projectIDE-record-last-modify record1)
                                       (projectIDE-record-last-modify record1))))
    (setq project-name-list
          (mapcar* 'concat
                   (mapcar 'projectIDE-record-name record-list) ;; project name
                   (mapcar (lambda (entry) (concat projectIDE-name-path-seperator entry)) ;; project path
                           (mapcar 'projectIDE-record-path record-list))))
    project-name-list))

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
    
    (let* ((choice (projectIDE-prompt "Choose project: " (projectIDE-get-project-list)))
           (path (progn
                   (string-match projectIDE-name-path-seperator choice)
                   (substring choice (match-end 0) nil)))
           (signature (projectIDE-get-signature-by-path path)))
      
      ;; Check if any unexpected result from user prompt
      (unless (and (file-exists-p path) (file-directory-p path))
        (projectIDE-message-handle 'Error
                                   (format "The path you tried to open is %s.
If it is unexpected, try modifying `projectIDE-name-path-seperator'" path)
                                   t
                                   'projectIDE-open-project
                                   (and projectIDE-debug-mode
                                        (nconc (list 'projectIDE-open-project) caller)))
        (throw 'Error nil))

      ;; Check whether project has been opened
      ;; If yes, just open the last opened buffer
      (when (gethash signature projectIDE-runtime-cache)
        (projectIDE-message-handle 'Info
                                   (format "Project [%s] had been opened already."
                                           (projectIDE-project-name (gethash signature projectIDE-opened-project)))
                                   t
                                   'projectIDE-open-project
                                   (and projectIDE-debug-mode
                                        (nconc (list 'projectIDE-open-project) caller)))
        (find-file (car (projectIDE-cache-opened-buffer (gethash signature projectIDE-runtime-cache))))
        (throw 'Error nil))
      
      ;; Check whether .projectIDE exists under path
      (unless (file-exists-p
               (concat (projectIDE-record-path (gethash signature projectIDE-runtime-record))
                       PROJECTIDE-PROJECTROOT-IDENTIFIER))
        (projectIDE-message-handle 'Warning
                                   (format ".projectIDE file not exists under %s.
If you moved the project, you can use `projectIDE-index-project' to reindex it."
                                           (projectIDE-record-path (gethash signature projectIDE-runtime-record)))
                                   t
                                   'projectIDE-open-project
                                   (and projectIDE-debug-mode
                                        (nconc (list 'projectIDE-open-project) caller)))
        (throw 'Error nil))

      (let* ((projectRoot (concat (projectIDE-record-path (gethash signature projectIDE-runtime-record)) PROJECTIDE-PROJECTROOT-IDENTIFIER))
             (project (projectIDE-parse-config projectRoot (and projectIDE-debug-mode
                                                                (nconc (list 'projectIDE-open-project) caller))))
             (cache nil))

        ;; check project parse successfully
        (unless project
          (projectIDE-message-handle
           'Info
           (format "Open project terminated due to unable reading config file %s" projectRoot)
           t
           'projectIDE-open-project
           (and projectIDE-debug-mode
                (nconc (list 'projectIDE-open-project) caller)))
          (throw 'Error nil))
        
        (puthash signature project projectIDE-opened-project)

        ;; check if there is cache file
        (unless (file-exists-p (concat PROJECTIDE-CACHE-PATH signature))
          (projectIDE-cache-create projectRoot))

        ;; check if cache load successfully
        (unless (fin>>projectIDE (concat PROJECTIDE-CACHE-PATH signature) 'cache (and projectIDE-debug-mode
                                                                                      (nconc (list 'projectIDE-open-project) caller)))
          (projectIDE-message-handle 'Error
                                     (format "Unable to load project cache: %s %s"
                                             (projectIDE-project-name project) signature)
                                     t
                                     'projectIDE-open-project
                                     (and projectIDE-debug-mode
                                          (nconc (list 'projectIDE-open-project) caller)))
          (throw 'Error nil))

        (when cache
          (puthash signature cache projectIDE-runtime-cache))
        
        ;; Try to open last opened files
        (when (and projectIDE-open-last-opened-files
                   (projectIDE-cache-opened-buffer cache))
          (let ((opened-buffer (projectIDE-cache-opened-buffer cache)))
            (setf (projectIDE-cache-opened-buffer cache) nil)
            (while (car-safe opened-buffer)
              (when (file-exists-p (car opened-buffer))
                (find-file (car opened-buffer)))
              (setq opened-buffer (cdr opened-buffer)))))
        
        ;; If no files are known to be opened
        ;; Open project root in dired mode
        (unless (projectIDE-cache-opened-buffer cache)
          (remhash signature projectIDE-opened-project)
          (remhash signature projectIDE-runtime-cache)
          (dired path))))))


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
             (unless (file-writable-p dir)
               (projectIDE-message-handle 'Error
                                          (format "Project directory \"%s\" is not writable." dir)
                                          t
                                          'projectIDE-create)
               (throw 'Error nil))
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
               (make-directory projectRoot)
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
             (projectIDE-project-signature (projectIDE-parse-config (concat path PROJECTIDE-PROJECTROOT-IDENTIFIER) nil
                                                                    (and projectIDE-debug-mode
                                                                         (nconc (list 'projectIDE-index-project) caller)))))
        (if (yes-or-no-p
             (concat (projectIDE-message-handle 'Warning
                                                (format ".projectIDE with signature found at %s" path)
                                                nil 'projectIDE-index-project)
                     "\nChoose yes if you want to create a new signature for this project.
Choose no if you want to retain current signature.
Press C-g to cancel the operation."))
            (projectIDE-new-project (file-name-as-directory path)
                                    (and projectIDE-debug-mode
                                         (nconc (list 'projectIDE-index-project) caller)))
          (projectIDE-record-create (concat path PROJECTIDE-PROJECTROOT-IDENTIFIER)
                                    (and projectIDE-debug-mode
                                         (nconc (list 'projectIDE-index-project) caller)))
          (projectIDE-cache-create (concat path PROJECTIDE-PROJECTROOT-IDENTIFIER)
                                   (and projectIDE-debug-mode
                                        (nconc (list 'projectIDE-index-project) caller))))
      (projectIDE-new-project (file-name-as-directory path)
                              (and projectIDE-debug-mode
                                   (nconc (list 'projectIDE-index-project) caller))))
    
    ;; Scan through buffers to check whether they are memeber of newly indexed project
    (let ((buffers (buffer-list)))
      (dolist (buffer buffers)
        (projectIDE-identify-project buffer (and projectIDE-debug-mode
                                                 (nconc (list 'projectIDE-index-project) caller)))))
    
    (projectIDE-message-handle 'Info
                               (format "Project Indexed\nProject\t\t\t\t: %s\nProject Directory\t: %s"
                                       (projectIDE-project-name
                                        (projectIDE-parse-config (concat path PROJECTIDE-PROJECTROOT-IDENTIFIER)))
                                       path)
                               t
                               'projectIDE-index-project
                               (and projectIDE-debug-mode
                                    (nconc (list 'projectIDE-index-project) caller)))))

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
          (setq projectIDE-opened-project (make-hash-table :test 'equal :size 20)
                projectIDE-runtime-cache (make-hash-table :test 'equal :size 20)
                projectIDE-buffer-trace (make-hash-table))
          (projectIDE-message-handle 'Info
                                     "projectIDE starts successfully."
                                     t
                                     'projectIDE-initialize)
          (add-hook 'find-file-hook 'projectIDE-identify-project)
          (add-hook 'kill-buffer-hook 'projectIDE-kill-project-buffer)
          (setq projectIDE-p t))
      (projectIDE-message-handle 'Error
                                 (format "projectIDE starts fail. Unable to read record file at %s" PROJECTIDE-RECORD-FILE)
                                 t
                                 'projectIDE-initialize))
    ;; Return value
    projectIDE-p))

(defun projectIDE-terminate ()
  "Terminate projectIDE"
  (interactive)
  (when projectIDE-p
    (remove-hook 'find-file-hook 'projectIDE-identify-project)
    (remove-hook 'kill-buffer-hook 'projectIDE-kill-project-buffer)
    (setq projectIDE-runtime-record nil 
          projectIDE-opened-project nil
          projectIDE-runtime-cache nil
          projectIDE-buffer-trace nil
          projectIDE-p nil)))

(provide 'projectIDE)
;;; projectIDE.el ends here
