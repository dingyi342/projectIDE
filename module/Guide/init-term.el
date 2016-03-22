;;; init-term.el --- project configuration file
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
;; This is a tutorial module.
;;

;;; code:



;; When you define a function called by initialize/terminate
;; You can never use projectIDE-defX
;; They should also be not interactive

(defun init-term-print-hello-when-initialize ()
  (message "Hello. Have a hard coding day."))

(defun init-term-print-goodbye-when-terminate ()
  (message "Goodbye. You become a superman when your job done."))


;; Funtion defined by projectIDE-defX
;; with a format packageName-initialize
;; and packageName-terminate will be called
;; when this module was first load or
;; when it is unload

(projectIDE-defun init-term-initialize ()
                  (init-term-print-hello-when-initialize))

(projectIDE-defun init-term-terminate ()
                  (init-term-print-goodbye-when-terminate))

;; Printing message when initialize and termiate is NOT suggested
;; This is only a demostration
;; It would make more sense if initialize or terminate
;; are used to read file or write to file.

(provide 'init-term)

;; init-term.el ends here
