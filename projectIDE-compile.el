;;; projectIDE-compile.el --- projectIDE compile
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
;; This files provides compile controlling funtions.
;;
;; Interactive functions:
;; projectIDE-compile
;;
;; Config file:
;; projectIDE-precompile
;; projectIDE-compile
;; projectIDE-postcompile
;; projectIDE-cleanup
;;
;;; code:
(require 'projectIDE-header)
(require 'projectIDE-debug)


;;;###autoload
(defun projectIDE-compile-generate-var-name (basevar prefix)
  
  "Generate a correct compile variable name from BASEVAR with prefix.

Return
Type:\t\t symbol
Descrip.:\t A correct compile variable name which can use to get data
\t\t\t from the .projectIDE config file.

BASEVAR
Type:\t\t symbol
Descrip.:\t The base variable name that what to be modified.

PREFIX
Type:\t\t int
Descrip.:\t The modifier of the base variable name."
  
  (cond
   ((eq basevar 'projectIDE-precompile-f)
    (if (= prefix 1)
        'projectIDE-precompile-f
      (intern (concat "projectIDE-precompile-f" (number-to-string prefix)))))
   ((eq basevar 'projectIDE-compile-f)
    (if (= prefix 1)
        'projectIDE-compile-f
      (intern (concat "projectIDE-compile-f" (number-to-string prefix)))))
   ((eq basevar 'projectIDE-postcompile-f)
    (if (= prefix 1)
        'projectIDE-postcompile-f
      (intern (concat "projectIDE-postcompile-f" (number-to-string prefix)))))
   ((eq basevar 'projectIDE-compile-cleanup-f)
    (if (= prefix 1)
        'projectIDE-compile-cleanup-f
      (intern (concat "projectIDE-compile-cleanup-f" (number-to-string prefix)))))))



;;;###autoload
(defun projectIDE-compile (arg)

  "Call the correct compile function which can be specified
in projectIDE config file by adding
\"projectIDE-compile = name-of-compile-function\".

For example, \"projectIDE-compile = cmake-compile\"
will call the 'cmake-compile function."
  
  (interactive "p")
  (catch 'Error
    (unless (projectIDE-get-Btrace-signature)
      (projectIDE-message 'Warning
                          "Current buffer is not part of a project."
                          t
                          (projectIDE-caller 'projectIDE-compiler))
      (throw 'Error nil))

    (save-some-buffers)
    (when (projectIDE-important-cmd-update-cache? (projectIDE-get-Btrace-signature))
         (projectIDE-update-cache))
        
    (with-temp-message
        (projectIDE-message 'Info
                            "Do not switch buffer during compile proecess."
                            nil)
      
     (let* ((signature (projectIDE-get-Btrace-signature))
           (precompile (projectIDE-get-module-var signature
                                                  (projectIDE-compile-generate-var-name 'projectIDE-precompile-f arg)))
           (compile (projectIDE-get-module-var signature
                                               (projectIDE-compile-generate-var-name 'projectIDE-compile-f arg)))
           (postcompile (projectIDE-get-module-var signature
                                                   (projectIDE-compile-generate-var-name 'projectIDE-postcompile-f arg)))
           (compile-cleanup (projectIDE-get-module-var signature
                                                       (projectIDE-compile-generate-var-name 'projectIDE-compile-cleanup-f arg))))
       
       (when (and (car-safe precompile) (fboundp (intern (car precompile))))
         (ignore-errors (apply (intern (car precompile)) (cdr precompile)))
         (when (projectIDE-important-cmd-update-cache? (projectIDE-get-Btrace-signature))
           (projectIDE-update-cache)))
              
       (if (and (car-safe compile) (fboundp (intern (car compile))))
           (apply (intern (car compile)) (cdr compile))
         (call-interactively 'compile))
       
       (when (and (car-safe postcompile) (fboundp (intern (car postcompile))))
         (when (projectIDE-important-cmd-update-cache? (projectIDE-get-Btrace-signature))
           (projectIDE-update-cache))
         (ignore-errors (apply (intern (car postcompile)) (cdr postcompile))))
       
       (when (and (car-safe compile-cleanup) (fboundp (intern (car compile-cleanup))))
         (when (projectIDE-important-cmd-update-cache? (projectIDE-get-Btrace-signature))
           (projectIDE-update-cache))
         (ignore-errors (apply (intern (car compile-cleanup)) (cdr compile-cleanup))))))

    (projectIDE-message 'Info
                        "Compile finished"
                        t)))


(projectIDE-register-Mx 'projectIDE-compile)


(provide 'projectIDE-compile)
;;; projectIDE-compile.el ends here
