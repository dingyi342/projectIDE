;;; projectIDE-dev.el --- project configuration file
;;
;; Copyright (C) 2015-2016 Mola-T
;; Author: Mola-T <Mola@molamola.xyz>
;; URL: https://github.com/mola-T/projectIDE
;; Version: 0.1
;; Keywords: project, convenience
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
;; This is part of projectIDE.el
;; This file provides functions for developers.
;;
;;; Code:

(defun projectIDE-print-variable (var)
  "Insert VAR at bottom of current buffer.
Only for debug purpose."
  (goto-char (point-max))
  (insert "\n\n")
  (let ((beg (point)))
    (save-excursion (insert (pp var))
                    (comment-region beg (point))))
  (message "Done"))

(defmacro pm (&rest args)
  "Print message."
  (if args
      (progn
        (message "[projectIDE::Debug] This is the start of debug message.\n")
        (dolist (arg args)
          (cond
           ((stringp arg)
            (pp arg))
           ((numberp arg)
            (pp arg))
           ((boundp arg)
            (pp arg)
            (message "")
            (pp (symbol-value arg)))
           ((not (boundp arg))
            (pp arg)
            (message "Undefined")))
          (message "\n"))
        (message "[projectIDE::Debug] This is the end of debug message."))
    (message "[projectIDE::Debug] This is a projectIDE debug message~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")))

(provide 'projectIDE-dev)
;;; projectIDE-dev.el ends here
