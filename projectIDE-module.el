;;; projectIDE-module.el --- project configuration file
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

;;; code:
(require 'cl-lib)
(require 'projectIDE-header)
(require 'projectIDE-debug)
(require 'projectIDE-fstream)

(defun projectIDE-load-module (signature)
  
  "Load module of project given by signature.

SIGNATURE
Type:\t\t string
Descrip.:\t A project based unique ID."
  
  (dolist (module (projectIDE-get-modules signature))
    (setq projectIDE-current-loading-module module)
    (require module nil t)
    (setq projectIDE-current-loading-module nil)))

(defun projectIDE-module-diff (signature-old signature-new)
  "Returns a list of module difference of two projects
specified by SIGNATURE-OLD and SIGNATURE-NEW.
The car of the returned list is the new modules needed to be added.
The cdr of the returned list is the modules neeeded to be diminished.
SIGNATURE-OLD or SIGNATURE-NEW can be nil.

Return
Type:\t\t list of two list
Descrip.:\t The car of the list is the modules needed to be added.
\t\t\t The cdr of the returned list is the modules neeeded to be diminished.

SIGNAUTRE-OLD or SIGNATURE-NEW
Type:\t\t string
Descrip.:\t Signature of project."
  
  (let ((modules-old (projectIDE-get-modules signature-old))
        (modules-new (projectIDE-get-modules signature-new))
        plus
        minus)
    (dolist (module modules-new)
      (unless (memq module modules-old)
        (push module plus)))
    (dolist (module modules-old)
      (unless (memq module modules-new)
        (push module minus)))
    (cons plus minus)))



(defun projectIDE-realize-defun (name)
  "Produce real function for `defun' type with NAME."
  (let* ((function (gethash name projectIDE-runtime-functions))
         (name (projectIDE-function-name function))
         (arglist (projectIDE-function-args function))
         (docstring (projectIDE-function-docstring function))
         (body (projectIDE-function-body function))
         (interactive (and (eq (caar body) 'interactive) (car body)))
         (body-1 (or (and interactive (append (list 'progn) (cdr body)))
                     (append (list 'progn) body))))
    (eval `(defun ,name ,arglist ,docstring ,interactive ,body-1))))

(defun projectIDE-realize-cl-defun (name)
  "Produce real function for `cl-defun' type with NAME."
  (let* ((function (gethash name projectIDE-runtime-functions))
         (name (projectIDE-function-name function))
         (arglist (projectIDE-function-args function))
         (docstring (projectIDE-function-docstring function))
         (body (projectIDE-function-body function))
         (interactive (and (eq (caar body) 'interactive) (car body)))
         (body-1 (or (and interactive (append (list 'progn) (cdr body)))
                     (append (list 'progn) body))))
    (eval `(cl-defun ,name ,arglist ,docstring ,interactive ,body-1))))

(defun projectIDE-realize-defmacro (name)
  "Produce real macro for `demacro' type with NAME."
  (let* ((function (gethash name projectIDE-runtime-functions))
         (name (projectIDE-function-name function))
         (arglist (projectIDE-function-args function))
         (docstring (projectIDE-function-docstring function))
         (body (car (projectIDE-function-body function))))
    (eval `(defmacro ,name ,arglist ,docstring ,body))))

(defun projectIDE-realize-cl-defmacro (name)
  "Produce real macro for `cl-demacro' type with NAME."
  (let* ((function (gethash name projectIDE-runtime-functions))
         (name (projectIDE-function-name function))
         (arglist (projectIDE-function-args function))
         (docstring (projectIDE-function-docstring function))
         (body (car (projectIDE-function-body function))))
    (eval `(cl-defmacro ,name ,arglist ,docstring ,body))))
    

(provide 'projectIDE-module)
;;; projectIDE-module.el ends here
