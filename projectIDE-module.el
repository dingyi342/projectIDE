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

(defun projectIDE-advice-buffer-change (&rest args)
  ;; In reality, to improve performance
  ;; advice after set-buffer instead of switch-to-buffer
  ;; advice after kill-buffer instead of adding to kill-buffer-hook
"Designed to be adviced after `switch-to-buffer'
add to `kill-buffer-hook'."

(unless projectIDE-renew-modules-timer
  (setq projectIDE-renew-modules-timer
        (run-with-idle-timer 0 nil 'projectIDE-module-controller))))



(defun projectIDE-module-controller ()
  
  "Control module activation and deactivation.
It is designed to work with `kill-buffer-hook' and
`switch-to-buffer'.

However, do not advice/hook this funcion directly.
Because it uses `curret-buffer' to resolve the
current module which may be invalid during the process.

Instead, it should run with idle timer.
See `projectIDE-advice-buffer-change' for details."

  ;; don't run on temp buffer
  (unless (string-match "\\*.*\\*" (buffer-name (current-buffer)))
    (setq projectIDE-active-project (projectIDE-get-Btrace-signature))
    (let ((diff (projectIDE-module-diff (projectIDE-get-Btrace-signature))))
      (dolist (plus (car diff))
        (projectIDE-activate-module plus))
      (dolist (minus (cdr diff))
        (projectIDE-deactivate-module minus))))
  (setq projectIDE-renew-modules-timer nil))



(defun projectIDE-load-module (name)
  
  "Attempt to load module given by NAME.
If module does not exist, just omits it.

NAME
Type:\t\t symbol
Descrip.:\t Symbol of the module name."

  (setq projectIDE-current-loading-module name)
  (require name nil t)
  (let ((initializer (intern (concat (symbol-name name) "-initialize"))))
    (when (projectIDE-get-function-object initializer)
      (projectIDE-realize-function initializer)
      (funcall initializer)))
  (setq projectIDE-current-loading-module nil))



(defun projectIDE-unload-module (name)
  
  "Attempt to unload module given by NAME.
If module did not, just omits it.

NAME
Type:\t\t symbol
Descrip.:\t Symbol of the module name."

  ;; Run the module terminate function
  (let ((terminator (intern (concat (symbol-name name) "-terminate"))))
    (when (projectIDE-get-function-object terminator)
      (projectIDE-realize-function terminator)
      (funcall terminator)))

  ;; Unbind all functions in module
  ;; Remove function records in projectIDE-runtime-functions
  (let ((functions (projectIDE-get-all-functions-from-module name)))
    (dolist (function functions)
      (fmakunbound function)
      (remhash function projectIDE-runtime-functions)))

  ;; Remove module record
  (cl-remf projectIDE-runtime-modules name)

  ;; Unload the whole package
  (with-demoted-errors (unload-feature name)))



(defun projectIDE-activate-module (module)
  
  "Activate MODULE.

MODULE
Type:\t\t symbol
Descrip.:\t Symbol of the module name."

   (let ((functions (projectIDE-get-all-functions-from-module module)))
    (dolist (function functions)
      (projectIDE-realize-function function)
      (projectIDE-realize-key function)))
   (cl-pushnew module projectIDE-active-modules))



(defun projectIDE-deactivate-module (module)
  
  "Deactivate MODULE.

MODULE
Type:\t\t symbol
Descrip.:\t Symbol of the module name."

   (let ((functions (projectIDE-get-all-functions-from-module module)))
     (dolist (function functions)
       (projectIDE-unrealize-key function)
       (fmakunbound function)))
   (setq projectIDE-active-modules (cl-remove module projectIDE-active-modules)))



(defun projectIDE-load-all-modules (signature)
  
  "Load module of project given by signature.

SIGNATURE
Type:\t\t string
Descrip.:\t A project based unique ID."
  
  (dolist (module (projectIDE-get-modules signature))
    (projectIDE-load-module module)))



(defun projectIDE-module-diff (signature)
  
  "Returns a list of module difference from
`projectIDE-active-modules' to module used by project
specified by SIGNATURE.

The car of the returned list is the new modules needed to be added.
The cdr of the returned list is the modules neeeded to be diminished.
SIGNATURE can be nil, meaning that no modules to compare to,
resulting a return list emphasize only on diminishing modules.

Return
Type:\t\t list of two list
Descrip.:\t The car of the list is the modules needed to be added.
\t\t\t The cdr of the returned list is the modules neeeded to be diminished.

SIGNAUTRE
Type:\t\t string
Descrip.:\t Signature of project."
  
  (let ((modules (projectIDE-get-modules signature))
        plus
        minus)
    (dolist (module modules)
      (unless (memq module projectIDE-active-modules)
        (push module plus)))
    (dolist (module projectIDE-active-modules)
      (unless (memq module modules)
        (push module minus)))
    (cons plus minus)))





(defun projectIDE-realize-function (name)
  
  "Attempt to realize the function given by NAME.
If NAME cannot be found in `projectIDE-runtime-functions', return nil."
  
  (let ((type (projectIDE-get-function-object-type name)))
    (cond
     ((eq type 'defun)
      (projectIDE-realize-defun name))
     ((eq type 'cl-defun)
      (projectIDE-realize-cl-defun name))
     ((eq type 'defmacro)
      (projectIDE-realize-defmacro name))
     ((eq type 'cl-defmacro)
      (projectIDE-realize-cl-defmacro name))
     (t nil))))

(defun projectIDE-realize-defun (name)
  "Produce real function for `defun' type with NAME."
  (let* ((function (projectIDE-get-function-object name))
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
  (let* ((function (projectIDE-get-function-object name))
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
  (let* ((function (projectIDE-get-function-object name))
         (name (projectIDE-function-name function))
         (arglist (projectIDE-function-args function))
         (docstring (projectIDE-function-docstring function))
         (body (car (projectIDE-function-body function))))
    (eval `(defmacro ,name ,arglist ,docstring ,body))))

(defun projectIDE-realize-cl-defmacro (name)
  "Produce real macro for `cl-demacro' type with NAME."
  (let* ((function (projectIDE-get-function-object name))
         (name (projectIDE-function-name function))
         (arglist (projectIDE-function-args function))
         (docstring (projectIDE-function-docstring function))
         (body (car (projectIDE-function-body function))))
    (eval `(cl-defmacro ,name ,arglist ,docstring ,body))))

(defun projectIDE-realize-key (function)
  "Produce real key bind for FUNCTION if it exists."
  (let ((key (projectIDE-get-function-key function)))
    (when key
      (define-key projectIDE-keymap key function))))

(defun projectIDE-unrealize-key (function)
  "Deactive key bind for FUNCTION if it exists."
  (let ((key (projectIDE-get-function-key function)))
    (when key
      (define-key projectIDE-keymap key nil))))
    

(provide 'projectIDE-module)
;;; projectIDE-module.el ends here
