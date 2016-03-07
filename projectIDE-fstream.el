;;; projectIDE-fstream.el --- project configuration file
;;
;;
;; Copyright (C) 2015-2016 Mola-T
;; Author: Mola-T <Mola@molamola.xyz>
;; URL: https://github.com/mola-T/projectIDE
;; Version: 0.1
;; Keywords: project, convenience
;;
;;
;;; License:
;;
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
;;
;; This is part of projectIDE.el
;; This file provides fstream functions.
;;
;;; Code:

(require 'projectIDE-header)
(require 'projectIDE-debug)

(defun fout<<projectIDE (file data &optional caller)
       "Write FILE with DATA.
DATA is any symbol variable in Emacs.

CALLER is the function list calling this function.
It is uesed for debugging purpose.

This function is safe.
It checks the accessibilty of file.
It will attempt to create the parent directories and the file
if they do not exist.
Return NIL if file is not accessable after all.

It checks the data for a valid symbol.
Return NIL if data value is void.

Return t if wirting file successfully.

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
Descrip.:\t Serialize data holds by symbol.

CALLER
Type:\t\t symbol list
Descrip.:\t Function list calling this function for debug purpose."
       
       (catch 'Error
         (let ((noError t))
           ;; Check file accessibility
           (unless (file-writable-p file)
             (let ((parentPath (file-name-directory file)))
               (when (or (file-directory-p parentPath)
                         (make-directory parentPath t)
                         (file-directory-p parentPath))
                 (write-region "" nil file nil 'inhibit nil 'exc1))
               (unless (file-writable-p file)
                 (projectIDE-message-handle 'Error
                                            (format "Unable to write to %s." file)
                                            nil
                                            'fout<<projectIDE
                                            (nconc (list 'fout<<projectIDE) caller))
                 (throw 'Error nil))))

           ;; Check symbol exists
           (unless (boundp data)
             (projectIDE-message-handle 'Error
                                        (format "Symbol %s is undefined." (symbol-name data))
                                        nil
                                        'fout<<projectIDE
                                        (nconc (list 'fout<<projectIDE) caller))
             (throw 'Error nil))

           ;; Serialize and wirte data to file
           (with-temp-file file
             (insert (prin1-to-string (symbol-value data))))

           (when (and projectIDE-debug-mode noError)
             (projectIDE-message-handle 'Info
                                        (format "Data %s written to file %s" (symbol-name data) file)
                                        nil
                                        'fout<<projectIDE
                                        (nconc (list 'fout<<projectIDE) caller)))
           noError)))

(defun fin>>projectIDE (file symbol &optional caller)
  "This function is safe.
It read data from FILE and restore data to SYMBOL.

CALLER is the function list calling this function.
It is uesed for debugging purpose.

It returns t it there isn't any error
It returns nil if data cannot be restored.

Return
Type:\t\t bool
Descrip.:\t t for no error, nil for error.

FILE
Type:\t\t string
Descrip.:\t Path to input file.
Example:\t ~/.emacs.d/file.txt , ~/usr/mola/documents/cache.txt

CALLER
Type:\t\t symbol list
Descrip.:\t Function list calling this function for debug purpose."
  
  (catch 'Error
    ;; Check file accessibility
    (unless (file-readable-p file)
      (projectIDE-message-handle 'Error
                                 (format "File %s is not accessible" file)
                                 nil
                                 'fin>>projectIDE
                                 (nconc (list 'fin>>projectIDE) caller))
      (throw 'Error nil))

    ;; Read from file
    (with-temp-buffer
      (insert-file-contents file)
      (unless (equal (point-min) (point-max))
        (if (boundp symbol)
            (set symbol (read (buffer-string)))
          (projectIDE-message-handle 'Error
                                     (format "Symbol %s is undefined." (symbol-name symbol))
                                     nil
                                     'fin>>projectIDE
                                     (nconc (list 'fin>>projectIDE) caller))
          (throw 'Error nil))))

    (when projectIDE-debug-mode
      (projectIDE-message-handle 'Info
                                 (format "Data from %s read into %s" file (symbol-name symbol))
                                 nil
                                 'fin>>projectIDE
                                 (nconc (list 'fin>>projectIDE) caller)))
    t))

(provide 'projectIDE-fstream)
;;; projectIDE-fstream.el ends here
