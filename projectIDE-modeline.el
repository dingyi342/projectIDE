;;; projectIDE-modeline.el --- projectIDE header file
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
;; This file is a part of projectIDE.
;; It modifies the mode line when current buffer is in a project.
;;
;;; code:
(require 'projectIDE-header)

(defcustom projectIDE-enable-project-mode-line t
  "Enable project mode line"
  :tag "Enable project mode line?"
  :type 'bool
  :group 'projectIDE-global)

(defvar projectIDE-mode-line-buffer-identification-failback
  mode-line-buffer-identification)

(defface projectIDE-project-name-face
  '((((class color) (min-colors 88) (background light))
     :background "gray70"
     :foreground "DodgerBlue")
    (((class color) (min-colors 88) (background dark))
     :background "gray30"
     :foreground "cyan"))
  "Basic face for highlighting.")

(defface projectIDE-project-folder-face
  '((((class color) (min-colors 88) (background light))
     :background "gray70"
     :foreground "gray20")
    (((class color) (min-colors 88) (background dark))
     :background "gray30"
     :foreground "gray80"))
  "Basic face for highlighting.")

(defface projectIDE-project-file-face
  '((((class color) (min-colors 88) (background light))
     :background "gray70"
     :foreground "yellow")
    (((class color) (min-colors 88) (background dark))
     :background "gray30"
     :foreground "yellow"))
  "Basic face for highlighting.")

(defun projectIDE-generate-mode-line ()
  
  "Return a mode line if current buffer is in a project.
Otherwise return nil."
  
  (if (projectIDE-get-Btrace-signature)
      (let* ((projectname (projectIDE-get-project-name (projectIDE-get-Btrace-signature)))
             (file-no-project-path (string-remove-prefix (projectIDE-get-project-path (projectIDE-get-Btrace-signature))(buffer-file-name)))
             (path (file-name-directory file-no-project-path))
             (file (file-name-nondirectory file-no-project-path)))
        (list
         (concat
          (propertize (concat "[" projectname "] ") 'face 'projectIDE-project-name-face)
          (and path (propertize path 'face 'projectIDE-project-folder-face))
          (propertize file 'face 'projectIDE-project-file-face))))
    nil))

(defvar projectIDE-mode-line-buffer-identification
  '(:eval (or (projectIDE-generate-mode-line)
              projectIDE-mode-line-buffer-identification-failback)))

(provide 'projectIDE-modeline)
;;; projectIDE-modeline.el ends here
