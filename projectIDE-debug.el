;;; projectIDE-debug.el --- project configuration file
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
;; This file provides debug functions.
;;
;;; Code:
(require 'projectIDE-header)

(defun projectIDE-message (type message &optional print functions)
  "This funtion handle messages from projectIDE for debug purpose.
TYPE specifies message type.  It can be 'Info, 'Warning or 'Error.
Warning and Error message will be logged by default for debug purpose.
Logging level can be set at `projectIDE-log-level'
MESSAGE elaborates the Info, Warning or Error.
PRINT controls whether message should be output to screen.
FUNCTION denotes the function calling this message handle.
FUNCTIONS denotes the functin calling chain.
It should be used by 'Warning or 'Error level for debug purpose.

TYPE
Type:\t\t symbol
Descrip.:\t Can be 'Error, 'Warning or 'Info to indicate message type.

MESSAGE
Type:\t\t string
Descrip.:\t Debug or informational message.

PRINT
Type:\t\t bool
Descrip.:\t Display message to minibuffer if it is t.
\t\t\t It is nil by default

FUNCTION
TYpe:\t\t symbol
Descrip.: Function producing the message.  Just for debug purpose.

FUNCTIONS
TYpe:\t\t symbol list
Descrip.: Functions calling FUNCTION to produce the message.  Just for debug purpose."
  
  (let ((Day (format-time-string "%Y%m%d"))
        (Time (format-time-string "%R"))
        message-prefix
        logtype)
    
    (cond ((eq type 'Error)
           (setq message-prefix "[projectIDE::Error]")
           (setq logtype 3))
          ((eq type 'Warning)
           (setq message-prefix "[projectIDE::Warning]")
           (setq logtype 2))
          (t
           (setq message-prefix "[projectIDE::Info]")
           (setq logtype 1)))

    (setq projectIDE-last-message (concat message-prefix " " message))
    
    (when print
      (message projectIDE-last-message))
    
    (when (and (file-exists-p PROJECTIDE-LOG-PATH)
               (or projectIDE-debug-mode (and logtype (>= logtype projectIDE-log-level))))
      (let ((message (replace-regexp-in-string "\n" "\n\t\t\t\t\t" message))
            functions-string)
        (dolist (function functions)
          (when (symbolp function)
            (setq functions-string (concat (symbol-name function) ">\n\t\t\t\t\t" functions-string))))
        
        (write-region
         (concat Day "-" Time "\t"
                 message-prefix "\t"
                 functions-string
                 message "\n")
         nil (concat PROJECTIDE-LOG-PATH Day ".log") t 'inhibit)))
    
    projectIDE-last-message))

(defmacro projectIDE-caller (current &optional caller)
  "Macro producing a list of function call chain.

CURRENT
Type:\t\t symbol
Descrip.:\t Current function name.

CALLER
Type:\t\t list of symbol
Descrip.:\t The list of function calling this function."
  
  `(nconc
    (list ,current)
    (and projectIDE-debug-mode ,caller)))

(defun projectIDE-debug-mode-on ()
  "An interative function to turn on debug mode.
Debug mode only print extra infomation to log file."
  (interactive)
  (setq projectIDE-debug-mode t))

(defun projectIDE-debug-mode-off ()
  "An interative function to turn off debug mode.
Debug mode only print extra infomation to log file."
  (interactive)
  (setq projectIDE-debug-mode nil))

(provide 'projectIDE-debug)
;;; projectIDE-debug.el ends here
