;;; projectAPI.el --- projectIDE API file
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
;; This files provides API for developing projectIDE modules.
;;
;;; code:
(require 'cl-lib)
(require 'projectIDE)


(defun projectAPI-p ()
  
  "Return t if projectIDE is running, otherwise nil."
  
  projectIDE-p)


(defalias 'projectAPI-init-maybe 'projectIDE-initialize-maybe

  "Try to initialize projectIDE if it is not running.
Do nothing if projectIDE is already running.
Return t if projectIDE is initialized.")



(defalias 'projectAPI-current-buffer-project 'projectIDE-get-Btrace-signature
  
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
\t\t\t  A project based unique ID.")



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



(defun projectAPI-get-var (var &optional signature)

  "Get config file variable with name VAR from project given by SIGNATURE.

Return
Type:\t\t list of string or nil
Descrip.:\t If VAR has been defined in config file,
\t\t\t string in list will be return.
\t\t\t If VAR has not been defined, nil will be return.
Example:\t In config file:
\t\t\t my-greeting = Hello have a nice day

\t\t\t (projectAPI-get-var (projectAPI-active-project) 'my-greeting)
\t\t\t ==>(\"Hello\" \"have\" \"a\" \"nice\" \"day\")

VAR
Type:\t\t symbol
Descrip.:\t Name of the variable.

SIGNATURE
Type:\t\t string
Descrip.:\t  A project based unique ID.
\t\t\t If SIGNATURE is not provided, `projectAPI-active-project' is used."

  (if (and var (symbolp var))
      (projectIDE-get-module-var (or signature projectIDE-active-project)
                                 var)
    (error "VAR need to be a symbol")))



(defun projectAPI-set-project-persist-memory (var value &optional signature)
  
  "Set VAR to VALUE in persist memory of project specified by SIGNATURE.
The variable set by this function is
1. project specific
2. maintain across Emacs sections.
The value set can get back by `projectAPI-get-project-persist-memory'.

VAR
Type:\t\t symbol
Descrip.:\t Name of variable.

VALUE
Type:\t\t any
Descrip.:\t Value of variable want to set.

SIGNATURE
Type:\t\t string
Descrip.:\t  A project based unique ID.
\t\t\t If SIGNATURE is not provided, `projectAPI-active-project' is used."
  
  (if (and var (symbolp var))
   (projectIDE-set-module-persist-memory (or signature projectIDE-active-project)
                                        var
                                        value)
   (error "VAR need to be a symbol")))



(defun projectAPI-get-project-persist-memory (var &optional signature)

  "Get value of VAR set by `projectAPI-set-project-persist-memory'.
If SIGNATURE is not provided, `projectAPI-active-project' is used.

Return
Type:\t\t any
Descrip.:\t Value of VAR set by `projectAPI-set-project-persist-memory'.

VAR
Type:\t\t symbol
Descrip.:\t Name of variable.

SIGNATURE
Type:\t\t string
Descrip.:\t  A project based unique ID.
\t\t\t If SIGNATURE is not provided, `projectAPI-active-project' is used."

  (if (and var (symbolp var))
      (projectIDE-get-module-persist-memory (or signature projectIDE-active-project)
                                            var)
    (error "VAR need to be a symbol")))


(defun projectAPI-set-project-nonpersist-memory (var value &optional signature)

  "Set VAR to VALUE in non-persist memory of project specified by SIGNATURE.
The variable set by this function is
1. project specific
2. cleaned up each Emacs session.
The value set can get back by `projectAPI-get-project-nonpersist-memory'.

VAR
Type:\t\t symbol
Descrip.:\t Name of variable.

VALUE
Type:\t\t any
Descrip.:\t Value of variable want to set.

SIGNATURE
Type:\t\t string
Descrip.:\t  A project based unique ID.
\t\t\t If SIGNATURE is not provided, `projectAPI-active-project' is used."
  
  (if (symbolp var)
   (projectIDE-set-module-nonpersist-memory (or signature projectIDE-active-project)
                                        var
                                        value)
   (error "VAR need to be a symbol")))



(defun projectAPI-get-project-nonpersist-memory (var &optional signature)

  "Get value of VAR set by `projectAPI-set-project-nonpersist-memory'.
If SIGNATURE is not provided, `projectAPI-active-project' is used.

Return
Type:\t\t any
Descrip.:\t Value of VAR set by `projectAPI-set-project-nonpersist-memory'.

VAR
Type:\t\t symbol
Descrip.:\t Name of variable.

SIGNATURE
Type:\t\t string
Descrip.:\t  A project based unique ID.
\t\t\t If SIGNATURE is not provided, `projectAPI-active-project' is used."

  (if (and var (symbolp var))
      (projectIDE-get-module-nonpersist-memory (or signature projectIDE-active-project)
                                            var)
    (error "VAR need to be a symbol")))



(defun projectAPI-set-persist-memory (var value)

  "Set VAR to VALUE in persist memory in global.
The variable set by this function is
1. global
2. maintained across Emacs session.
The value set can get back by `projectAPI-get-persist-memory'.

VAR
Type:\t\t symbol
Descrip.:\t Name of variable.

VALUE
Type:\t\t any
Descrip.:\t Value of variable want to set."
  
  (if (and var (symbolp var))
      (projectIDE-set-persist-memory var value)
    (error "VAR need to be a symbol")))



(defun projectAPI-get-persist-memory (var)

  "Get value of VAR set by `projectAPI-set-persist-memory'.

Return
Type:\t\t any
Descrip.:\t Value of VAR set by `projectAPI-set-persist-memory'.

VAR
Type:\t\t symbol
Descrip.:\t Name of variable."

  (if (and var (symbolp var))
      (projectIDE-get-persist-memory var)
    (error "VAR need to be a symbol")))



(defun projectAPI-set-nonpersist-memory (var value)

  "Set VAR to VALUE in non-persist memory in global.
The variable set by this function is
1. global
2. cleaned up each Emacs session.
The value set can get back by `projectAPI-get-nonpersist-memory'.

VAR
Type:\t\t symbol
Descrip.:\t Name of variable.

VALUE
Type:\t\t any
Descrip.:\t Value of variable want to set."
  
  (if (symbolp var)
      (projectIDE-set-nonpersist-memory var value)
    (error "VAR need to be a symbol")))



(defun projectAPI-get-nonpersist-memory (var)

  "Get value of VAR set by `projectAPI-set-nonpersist-memory'.

Return
Type:\t\t any
Descrip.:\t Value of VAR set by `projectAPI-set-nonpersist-memory'.

VAR
Type:\t\t symbol
Descrip.:\t Name of variable."

  (if (and var (symbolp var))
      (projectIDE-get-nonpersist-memory var)
    (error "VAR need to be a symbol")))



(defalias 'projectAPI-defun 'projectIDE-defun
  
  "A project specific `defun'.

Functions defined through `projectAPE-defun' will be active
if and only if the project requiring the module is in active state.
It is managed by projectIDE.

User can use `projectIDE-mod-key' to set keybind for function
defined by `projectAPI-defun'. Keybind will be active IFF
the project requiring the module is in active state.

It is recommended only `interactive' functions are set
by `projectAPI-defun' because non interactive function is usually
invisible to user. The effect of activate and deactivate such an
'invisible' function is not obvious.

Functions defined through `projectAPI-defun' does not require
to do `projectAPI-register-Mx' as they are registered automatically.")



(defalias 'projectAPI-cl-defun 'projectIDE-cl-defun
  
  "A project specific `cl-defun'.

Functions defined through `projectAPE-cl-defun' will be active
if and only if the project requiring the module is in active state.
It is managed by projectIDE.

User can use `projectIDE-mod-key' to set keybind for function
defined by `projectAPI-cl-defun'. Keybind will be active IFF
the project requiring the module is in active state.

It is recommended only `interactive' functions are set
by `projectAPI-cl-defun' because non interactive function is usually
invisible to user. The effect of activate and deactivate such an
'invisible' function is not obvious.

Functions defined through `projectAPI-cl-defun' does not require
to do `projectAPI-register-Mx' as they are registered automatically.")



(defun projectAPI-register-Mx (functions)

  "Register FUNCTIONS to `projectIDE-M-x-functions'
so that functions appear in `projectIDE-M-x'.
FUNCTIONS can be a list of functions or just a single function.

FUNCTIONS
Type:		 symbol or symbol list
Descrip.:	 Register to `projectIDE-M-x-functions'."

  (if (listp functions)
      (dolist (function functions)
        (unless (and function (fboundp function))
          (error "Function %s is not defined" (symbol-name function))))
    (unless (and functions (fboundp functions))
      (error "Function %s is not defined" (symbol-name functions))))

  (projectIDE-register-Mx functions))

;; Functions that not provided at this moment.
;; (defalias 'projectAPI-defmacro 'projectIDE-defmacro
;;   "A project specific `defmacro'.")
;; (defalias 'projectAPI-cl-defmacro 'projectIDE-defmacro
;;   "A project specific `defmacro'.")
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
