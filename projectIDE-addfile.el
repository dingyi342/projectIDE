;;; projectIDE-addfile.el --- projectIDE addfile
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
;; This is part of projectIDE.
;; This files provides file adding funtions.
;;
;; Interactive functions:
;; projectIDE-addfile
;;
;; Config file:
;; projectIDE-addfile(X)
;;
;;
;;; code:
(require 'projectIDE-header)

;;;###autoload
(defun projectIDE-addfile-generate-var-name (basevar prefix)
  
  "Generate a correct addfile variable name from BASEVAR with prefix.

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
  
  (if (= prefix 1)
      'projectIDE-addfile
    (intern (concat "projectIDE-addfile" (number-to-string prefix)))))



;;;###autoload
(defun projectIDE-parse-addfile (elt)

  "Parse the addfile variable ELT.
The original addfile variable should be either \"template::${name}.ext\"
or simply \"${name}.ext\". Seperate the tempate and the file and return
them as a cons cell.

Return
Type:\t\t cons cell
Descrip.:\t car is the template. cdr is the file.

ELT
Type:\t\t string
Descrip.:\t Unparsed addfile variable."
  
  (let (template
        filepath)
    (if (string-match "::" elt)
        (progn
          (setq template (substring elt 0 (string-match "::" elt)))
          (setq filepath (substring elt (and (string-match "::" elt) (match-end 0)) nil)))
      (setq filepath elt))

    (unless (file-exists-p template)
      (setq template nil))

    (cons template filepath)))



;;;###autoload
(defun projectIDE-addfile-destination (projectRoot vardest filename)
  
  "Replace \"${name}\" in VARDEST with FILENAME.
Return the result with PROJECTROOT added in fornt.

Return
Type:\t\t string
Descrip.:\t Manipulated file destination.

PROJECTROOT
Type:\t\t string
Descrip.:\t Project root.

VARDEST
Type:\t\t string
Descrip.:\t Filename with ${name} to be subsituted.

FILENAME
Type:\t\t string
Descrip.:\t Filename to subsitute ${name}."
  
  (concat projectRoot (replace-regexp-in-string "${name}" filename vardest nil t)))


;;;###autoload
(defun projectIDE-addfile (prefix)

  "Add file to current project.
Serval file can be added at the same time if
\"projectIDE-addfile = \" has been set in config file.

For Example:
\"projectIDE-addfile = template1::${name}.a template2::${name}.b\"
will add two files with template."
  
  (interactive "p")
  (let ((signature projectIDE-active-project)
        (projectRoot (projectIDE-get-project-path projectIDE-active-project))
        (commands
         (projectIDE-get-module-var projectIDE-active-project
                                    (projectIDE-addfile-generate-var-name 'projectIDE-addfile prefix)))
        (message "File(s) will be created at:\n")
        files
        name)

    (catch 'stop
      (unless commands
        (call-interactively 'find-file)
        (throw 'stop nil))

      (dolist (command commands)
        (push (projectIDE-parse-addfile command) files))
      (setq files (nreverse files))
      

      (dolist (file files)
        (setq message (concat message (cdr file) "\n")))
      (setq message (concat message "File name: "))

      (setq name (read-string
                  (projectIDE-message 'Info
                                      message
                                      nil
                                      (projectIDE-caller 'projectIDE-addfile))))
      (when (string= name "")
        (projectIDE-message 'Info
                            "Cannot create file with empty file name. Operation cancelled."
                            t
                            (projectIDE-caller 'projectIDE-addfile))
        (throw 'stop nil))

      (dolist (file files)
        (setcdr file (projectIDE-addfile-destination projectRoot (cdr file) name)))
      
      (dolist (file files)
        (if (and (car file) (file-exists-p (car file)) (not (file-directory-p (car file))))
            (unless (file-exists-p (cdr file))
              (unless (file-exists-p (file-name-directory (cdr file)))
                (mkdir (file-name-directory (cdr file)) t))
              (copy-file (car file) (cdr file)))
          (unless (file-exists-p (cdr file))
              (unless (file-exists-p (file-name-directory (cdr file)))
                (mkdir (file-name-directory (cdr file)) t))
              (write-region "" nil (cdr file))))
        (find-file (cdr file)))

      (projectIDE-message 'Info
                          "Files created successfully."
                          t
                          (projectIDE-caller 'projectIDE-addfile)))))

(projectIDE-register-Mx 'projectIDE-addfile)

(provide 'projectIDE-addfile)
;;; projectIDE-addfile.el ends here
