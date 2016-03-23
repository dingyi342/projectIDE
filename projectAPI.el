;;; projectAPI.el --- projectIDE API file
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
;; This files provides API for developing projectIDE modules.
;;
;;; code:
(require 'cl-lib)
(require 'projectIDE)


(defun projectAPI-p ()
  
  "Return t if projectIDE is running, otherwise nil."
  
  projectIDE-p)



(defun projectAPI-current-buffer-project ()
  
  "Return the signature of current buffer.
If current buffer is not a member of a project,
return nil.

It differs with `projectAPI-active-project' that
it returns a valid signature IFF the current buffer
is a member of a project, while `projectAPI-active-project'
can return a signature even the project buffer activated
a temp buffer.  See `projectAPI-active-project' for its detail.

Return
Type\t\t: string
Descrip.:\t A project signature.
\t\t\t  A project based unique ID."
  
  (projectIDE-get-Btrace-signature))

(defun projectAPI-active-project ()
  
  "Return the signature of active project.
If there is no active project, return nil.

A project is said to be active when
after a buffer from that project has been opened,
before it swith to non temporary buffer which is not a project
or which is another project.
For example, we have #<buffer project1>, #<buffer project2>
, #<buffer *Message*> and #<buffer foo>

#<buffer project1> to #<buffer *Message*>:
Active project = project1

#<buffer project1> to #<buffer project2>:
Active project = project2

#<buffer project1> to #<buffer foo>:
Active project = nil

It differs with `projectAPI-current-buffer-project' that
it returns the signature even if a temp buffer has been activated,
while `projectAPI-current-buffer-project' returns the signature
if and only if current buffer is a member of a project.
See `projectAPI-current-buffer-project' for more details.

Return
Type:\t\t string
Descrip.:\t A project signature.
\t\t\t  A project based unique ID."
  
  projectIDE-active-project)



(defun projectAPI-project-root (&optional signature)

  "Return the project root path of project given by SIGNATURE.
SIGNATURE can be obtained either from `projectAPI-current-buffer-project'
or `projectAPI-active-project'.
If SIGNATURE is not provided, `projectAPI-current-buffer-project' is used.
Return nil if SIGNATURE is invalid.

Return
Type:\t\t string or nil
Descrip.:\t Project name of project given by SIGNATURE.

SIGNATURE
Type:\t\t string
Descrip.:\t A project based unique ID."

  (projectIDE-get-project-path (or signature (projectIDE-get-Btrace-signature))))



(defun projectAPI-project-name (&optional signature)
  
  "Return the project name of project given by SIGNATURE.
SIGNATURE can be obtained either from `projectAPI-current-buffer-project'
or `projectAPI-active-project'.
If SIGNATURE is not provided, `projectAPI-current-buffer-project' is used.
Return nil if SIGNATURE is invalid.

Return
Type:\t\t string or nil
Descrip.:\t Project name of project given by SIGNATURE.

SIGNATURE
Type:\t\t string
Descrip.:\t A project based unique ID."
  
  (projectIDE-get-project-name
   (or signature (projectIDE-get-Btrace-signature))))



(defun projectAPI-project-buffers (&optional signature)

  "Return all opened buffer objects from project given by SIGNATURE.
SIGNATURE can be obtained either from `projectAPI-current-buffer-project'
or `projectAPI-active-project'.
If SIGNATURE is not provided, `projectAPI-current-buffer-project' is used.
Return nil if SIGNATURE is invalid.

Return
Type:\t\t List of Emacs buffer object or nil
Descrip.:\t Buffers of project given by SIGNATURE opened.

SIGNATURE
Type:\t\t string
Descrip.:\t A project based unique ID."

  (projectIDE-get-buffer-list (or signature (projectIDE-get-Btrace-signature))))



(defun projectAPI-project-files (&optional signature shortpath filter)

  "Return a list of files from project given by SIGNATURE.
If SIGNATURE is not provided `projectAPI-active-project' is used.
Return nil if SIGNATURE provided is invalid.

When SHORTPATH is non-nil, the return files list contains paths
relative to project root.

FILTER is a unary predicate funtion accepting one argument
to test each of the entry.  The predicate function should return t
if an element was consider to be returned.

Return
Type:\t\t list of string
Descrip.:\t List of file paths.

SIGNATURE
Type:\t\t string
Descrip.:\t  A project based unique ID.

SHORTPATH
Type:\t\t bool
Descrip.:\t non-nil indicate returning of short path.

FILTER
Type:\t\t symbol
Descrip.:\t Unary predicate function.
Example:\t (projectAPI-project-files
\t\t\t         nil nil (lambda (test) (if (string-match \"*.cpp\" test) nil t))))"

  (catch 'invalid-signature
    (when signature
      (unless (projectIDE-get-cache signature)
        (throw 'invalid-signature nil)))
    (projectIDE-get-file-list (or signature projectIDE-active-project)
                              (not shortpath)
                              filter
                              (projectIDE-caller 'projectAPI-project-files))))

(defun projectAPI-project-folders (&optional signature shortpath filter)

  "Return a list of folderw from project given by SIGNATURE.
If SIGNATURE is not provided `projectAPI-active-project' is used.
Return nil if SIGNATURE provided is invalid.

When SHORTPATH is non-nil, the return folder list contains paths
relative to project root.
FILTER is a unary predicate funtion accepting one argument
to test each of the entry.  The predicate function should return t
if an element was consider to be returned.

Return
Type:\t\t list of string
Descrip.:\t List of folder paths.

SIGNATURE
Type:\t\t string
Descrip.:\t  A project based unique ID.

SHORTPATH
Type:\t\t bool
Descrip.:\t non-nil indicate returning of short path.

FILTER
Type:\t\t symbol
Descrip.:\t Unary predicate function.
Example:\t (projectAPI-project-folders
\t\t\t         nil (lambda (test) (if (string-match \"\\lib\\\" test) nil t))))"

  (catch 'invalid-signature
    (when signature
      (unless (projectIDE-get-cache signature)
        (throw 'invalid-signature nil)))
    (projectIDE-get-folder-list (or signature projectIDE-active-project)
                                (not shortpath)
                                filter
                                (projectIDE-caller 'projectAPI-project-folders))))




(defalias 'projectAPI-defun 'projectIDE-defun
  "A project specific `defun'.")
(defalias 'projectAPI-cl-defun 'projectIDE-cl-defun
  "A project specific `cl-defun'.")
(defalias 'projectAPI-defmacro 'projectIDE-defmacro
  "A project specific `defmacro'.")
(defalias 'projectAPI-cl-defmacro 'projectIDE-defmacro
  "A project specific `defmacro'.")

;; Functions that not provided at this moment.
;; (defun projectAPI-update-cache-pre-prompt? ()
;;   "Return t if user want to update cache for `projectAPI-active-project'
;; before any prompt, otherwise nil."
;;   (projectIDE-pre-prompt-update-cache? projectIDE-active-project))
;; (defun projectAPI-update-cache-important-cmd? ()
;;   "Return t if user want to update cache for `projectAPI-active-project'
;; before any important command, otherwise nil."
;;   (projectIDE-important-cmd-update-cache? projectIDE-active-project))
;; (defun projectAPI-update-cache ()
;;   "Update cache for `projectAPI-active-project'.
;; This funcion is adviced NOT to be used unless extreme need."
;;   (projectIDE-update-cache))

(provide 'projectAPI)
;;; projectAPI.el ends here
