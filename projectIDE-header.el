(require 'cl-lib)

;;; projectIDE Group
(defgroup projectIDE nil
  "Managing projects like an IDE."
  :tag "projectIDE"
  :group 'tools
  :group 'convenience)
(defgroup projectIDE-global nil
  "Global setting for all projects."
  :tag "Enviroment Values"
  :group 'projectIDE)
(defgroup projectIDE-config-file nil
  "Setting for loading individual project config file."
  :tag "Config file settings"
  :group 'projectIDE)
(defgroup projectIDE-project-creation nil
  "Setting for creating project."
  :tag "Project creation"
  :group 'projectIDE)
(defgroup projectIDE-project-opening nil
  "Setting for opening project behaviour."
  :tag "Project opening"
  :group 'projectIDE)
(defgroup projectIDE-file-opening nil
  "Setting for opening project file behaviour."
  :tag "File opening"
  :group 'projectIDE)
(defgroup projectIDE-hook nil
  "All available projectIDE hooks."
  :tag "Hook"
  :group 'projectIDE)

;;; Variables
;; Environmental Variable
(defconst PROJECTIDE-PROJECTROOT-IDENTIFIER ".projectIDE"
  "The root file indicator.")

(defcustom projectIDE-database-path
  (file-name-as-directory
   (concat (file-name-as-directory user-emacs-directory) "projectIDE"))
  "Type: string\nPath for storing projectIDE RECORD database."
  :tag "Main database path"
  :type 'directory
  :group 'projectIDE-global)

(defconst PROJECTIDE-RECORD-FILE
  (concat projectIDE-database-path "RECORD")
  "Type: string\nFilename for project record.")

(defconst PROJECTIDE-LOG-PATH
  (file-name-as-directory (concat projectIDE-database-path "LOG"))
  "Type: string\nFilename for projectIDE log file.")


(defconst PROJECTIDE-CACHE-PATH
  (file-name-as-directory (concat projectIDE-database-path "CACHE"))
  "Type: string\nFolder path to individual project record.")

(defcustom projectIDE-completion-system
  (or (and (fboundp 'helm) 'helm)
      (and (fboundp 'ivy-completing-read) 'ivy)
      (and (fboundp 'ido-completing-read) 'ido)
      (and (fboundp 'grizzl-completing-read) 'grizzl)
      'default)
  "The completion system to be used by projectIDE."
  :type '(radio
          (const :tag "Ido" ido)
          (const :tag "Ivy" ivy)
          (const :tag "Grizzl" grizzl)
          (const :tag "Helm" helm)
          (const :tag "Default" default))
  :group 'projectIDE-global)

;; Project config file variable
(defconst projectIDE-default-config-key
  '("^signature="
    "^name="
    "^exclude="
    "^whitelist="
    "^cachemode="
    "^module=")
  "Default projectIDE config file keyword.
Must not change.")

(defconst projectIDE-CACHEMODE-open-project-update-cache 1
  "[00000001] Do a full update cache after first opening a project.")
(defconst projectIDE-CACHEMODE-background-update-cache 2
  "[00000010] Do background update cache from time to time
if project is opened.")
(defconst projectIDE-CACHEMODE-update-cache-pre-prompt 4
  "[00000100] Fully update cache before any promt.")
(defconst projectIDE-CACHEMODE-update-cache-important-command 8
  "[00001000] Fully update cache before important command like compile.")

(defcustom projectIDE-default-cachemode
  (logior projectIDE-CACHEMODE-open-project-update-cache
          projectIDE-CACHEMODE-background-update-cache
          projectIDE-CACHEMODE-update-cache-important-command)
  "Default cache mode for projects.
`projectIDE-CACHEMODE-open-project-update-cache' = 1
`projectIDE-CACHEMODE-background-update-cache' = 2
`projectIDE-CACHEMODE-update-cache-pre-prompt' = 4
`projectIDE-CACHEMODE-update-cache-important-command' = 8
A sum of these cache modes you want to enable.
"
  :tag "Default cache mode"
  :type 'integer
  :group 'projectIDE-config-file)

;; Project Creation Variable
(defcustom projectIDE-create-defaultDir (getenv "HOME")
  "Global default project directory.
When creating project, if no specific directory or
invalid default directory is entered, projectIDE uses this
variable as the default directory."
  :tag "Default project directory"
  :type 'directory
  :group 'projectIDE-project-creation)

(defcustom projectIDE-global-project-create-hook nil
  "Hook runs when creating project."
  :tag "projectIDE-global-project-create-hook"
  :type 'hook
  :group 'projectIDE-project-creation
  :group 'projectIDE-hook)

(defcustom projectIDE-create-require-confirm t
  "Require confirmation when creating project?
If this value is nil, projectIDE will skip confirmation
before creating project.
Other values ask for the confirmation."
  :tag "Require confirmation when creating project?"
  :type 'boolean
  :group 'projectIDE-project-creation)

;; Project Opening Variable
(defcustom projectIDE-name-path-seperator "\t\t\t\@PATH="
  "The seperator of name and path when opening project.
There must be an unique identifier like '@'
as it will be use to trim the path."
  :tag "Name-path seperator."
  :type 'string
  :group 'projectIDE-project-opening)

(defcustom projectIDE-open-last-opened-files t
  "When opening a project, open the last opened files instead of Dired."
  :tag "Open last opened files?"
  :type 'bool
  :group 'projectIDE-project-opening)

;; File Opening Variable
(defcustom projectIDE-use-project-prefix t
  "When showing files, show project name as prefix instead of full path."
  :tag "Use project prefix instead of full path?"
  :type 'bool
  :group 'projectIDE-file-opening)

(defcustom projectIDE-open-file-update-cache t
  "For the first time opening a file from a project, update the project cache."
  :tag "Update cache when first opening a file from a project?"
  :type 'bool
  :group 'projectIDE-file-opening)

;; Debug and logging variable
(defcustom projectIDE-log-level 2
  "Logging level specifies what to be logged."
  :tag "Logging level"
  :type '(radio
          (const :tag "Error" 3)
          (const :tag "Warning" 2)
          (const :tag "Info" 1)
          (const :tag "Disable" nil))
  :group 'projectIDE-global)


;; Runtime Variable
(defvar projectIDE-p nil
  "Indicate whether projectIDE is running.
Never attempt to modify it directly.")

(defvar projectIDE-debug-mode nil
  "Indicate whether projectIDE is in debug mode.
Debug mode only add extra output to log file.")

(defvar projectIDE-last-message nil
  "Record the last message.")

(defvar projectIDE-runtime-record nil
  ;; hash table
  ;; key: signature
  ;; value: project-record object
  "Database recording all project.
Never attempt to modify it directly.")

(defvar projectIDE-opened-project nil
  ;; hash table
  ;; key: signature
  ;; value: project object
  "Current opened project.
Never attempt to modify it directly.")

(defvar projectIDE-runtime-cache nil
  ;; hash table-backward-cell
  ;; key: signature
  ;; value: project cache object
  "Individual project cache.
Never attempt to modify it directly.")

(defvar projectIDE-buffer-trace
  ;; hash table
  ;; key: buffer
  ;; value: signature
  "Trace buffer which is a projectIDE project.
Never attempt to modify it directly.")

(defvar projectIDE-save-cache nil
  "To temporary store the file name to be saved at before-save hook.
And cache this file at at after-save-hook.")

(defun projectIDE-concat-regexp (list)
  "Return a single regexp string from a LIST of separated regexp.

Return
Type:\t\t string
Descrip.:\t A single string of combined regexp.
\t\t\t If LIST is empty, return nil.

LIST
Type:\t\t string list
Descrip.:\t A string list of regexp."
  
  (let (regexp)
    (dolist (val list regexp)
      (if regexp
          (setq regexp (concat val "\\|" regexp))
        (setq regexp val)))))

(defvar projectIDE-default-config-key-regexp-string
  (projectIDE-concat-regexp projectIDE-default-config-key)
  "Combine the projectIDE-default-exclude.")

(defcustom projectIDE-default-exclude
  `(,(concat (file-name-as-directory "*.idea") "*")
    ,(concat (file-name-as-directory "*.eunit") "*")
    ,(concat (file-name-as-directory "*.git") "*")
    ,(concat (file-name-as-directory "*.hg") "*")
    ,(concat (file-name-as-directory "*.fslckout") "*")
    ,(concat (file-name-as-directory "*.bzr") "*")
    ,(concat (file-name-as-directory "*_darcs") "*")
    ,(concat (file-name-as-directory "*.tox") "*")
    ,(concat (file-name-as-directory "*.svn") "*")
    ,(concat (file-name-as-directory "*.stack-work") "*"))
  "A list of exclude items by projectIDE."
  :group 'projectIDE-config-file
  :type '(repeat string))

(defcustom projectIDE-default-whitelist nil
    "A list of exclude items by projectIDE."
  :group 'projecIDE-config-file
  :type '(repeat string))

(defcustom projectIDE-config-file-search-up-level 4
  "Number of upper level directories to search for the .projectIDE file."
  :tag "Config file search up level"
  :type 'integer
  :group 'projectIDE-config-file)

;; Object




;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;; record object
(cl-defstruct projectIDE-record
  signature
  name
  path
  create-time
  modify-time
  reserve-field-1
  reserve-field-2
  reserve-field-3
  )

;;; Getter and setter function
;;fff (defun projectIDE-get-all-singatures ())
;;fff (defun projectIDE-get-all-records ())
;;fff (defun projectIDE-get-record (signature))
;;fff (defun projectIDE-get-signature-by-path (path &optional caller))
;;fff (defun projectIDE-get-project-name (signature))
;;fff (defun projectIDE-get-project-path (signature))
;;fff (defun projectIDE-get-project-create-time (signature))
;;fff (defun projectIDE-get-project-modify-time (signature))
;;fff (defun projectIDE-set-project-name (signature name))
;;fff (defun projectIDE-set-project-path (signature path))
;;fff (defun projectIDE-set-project-modify-time (signature))

(defun projectIDE-get-all-singatures ()
  "Get a list of all signatures found in projectIDE-runtime-record.

Return
Type:\t\t list of stirng or nil
Descrip.:\t A list of signature in string in projectIDE-runtime-record.
\t\t\tIf there is nothing in runtime record, return nil."
  (hash-table-keys projectIDE-runtime-record))

(defun projectIDE-get-all-records ()
  "Get a list of all records found in projectIDE-runtime-record.

Return
Type:\t\t list of record objects or nil
Descrip.:\t A list of record objects from projectIDE-runtime-record.
\t\t\tIf there is nothing in runtime record, return nil."
  (hash-table-values projectIDE-runtime-record))

(defun projectIDE-get-record (signature)
  "Get a reference to record object by SIGNATURE.
Return nil with it is unable to find that signature.

Return
Type:\t\t projectIDE-record object or nil
Descrip.:\t Return a record object if found or nil if not found.

SIGNATURE
Type:\t\t string
Descrip.:\t A project based unique ID."
  (gethash signature projectIDE-runtime-record))

(defun projectIDE-get-signature-by-path (path &optional caller)
  "Return signature by the best match of PATH in record.
Record is search in projectIDE-runtime-record.

CALLER is the function list calling this function.
It is uesed for debugging purpose.

Return
Type:\t\t projectIDE-record signature or nil
Descrip.:\t projectIDE-record signature of given PATH.
\t\t\t nil if PATH not found.

PATH
Type:\t\t string
Descript.:\t File or folder path in string.

CALLER
Type:\t\t symbol list
Descrip.:\t Function list calling this function for debug purpose."
  
  (let ((records (projectIDE-get-all-records))
        (candidates nil)
        (signature nil))

    ;; Search all recods matched path
    (dolist (record records)
      (when (string-prefix-p (projectIDE-record-path record) path)
        (add-to-list 'candidates (projectIDE-record-signature record))))

    (setq signature (car candidates))

    ;; Use the best match result
    (unless (<= (length candidates) 1)
      (dolist (candidate candidates)
        (when (> (length (projectIDE-get-project-path candidate))
                 (length (projectIDE-get-project-path signature)))
          (setq signature candidate))))

    (when projectIDE-debug-mode
      (projectIDE-message-handle 'Info
                                 (format "Try to find record under %s. Found: %s" path signature)
                                 nil
                                 'projectIDE-get-signature-by-path
                                 (nconc (list 'projectIDE-get-signature-by-path) caller)))
    signature))

(defun projectIDE-get-project-name (signature)
"Get the project name of given SIGNATURE.

Return
Type:\t\t string
Descrip.:\t\t Name of project of the given signature.

SIGNATURE
Type:\t\t string
Descrip.:\t A project based unique ID."
  (projectIDE-record-name (gethash signature projectIDE-runtime-record)))

(defun projectIDE-get-project-path (signature)
  "Get the project root path of given SIGNATURE.
Return nil if \".projectIDE\" no longer exists at path.

Return
Type:\t\t string or nil
Descrip.:\t\t Path to project root or nil if invalid project root.

SIGNATURE
Type:\t\t string
Descrip.:\t A project based unique ID."
  (let ((path (projectIDE-record-path (gethash signature projectIDE-runtime-record))))
    (if (file-readable-p (concat path PROJECTIDE-PROJECTROOT-IDENTIFIER))
        path
      nil)))

(defun projectIDE-get-config-file-path (signature)
  "Get the project config file path of given SIGNATURE.
Return nil if \".projectIDE\" no longer exists at path.

Return
Type:\t\t string or nil
Descrip.:\t\t Path to project config file
\t\t\t: Returns nil if config file no longer exists.

SIGNATURE
Type:\t\t string
Descrip.:\t A project based unique ID."
  (let ((path
         (concat (projectIDE-record-path
                  (gethash signature projectIDE-runtime-record))
                 PROJECTIDE-PROJECTROOT-IDENTIFIER)))
    (if (file-readable-p path)
        path
      nil)))

(defun projectIDE-get-project-create-time (signature)
  "Get the creation time of project given by SIGNATURE.

Return
Type:\t\t Emacs time
Descrip.:\t Date and time that the project created.

SIGNATURE
Type:\t\t string
Descrip.:\t A project based unique ID."

  (projectIDE-record-create-time (gethash signature projectIDE-runtime-record)))

(defun projectIDE-get-project-modify-time (signature)
  "Get the modify time of project given by SIGNATURE

Return
Type:\t\t Emacs time
Descrip.:\t Date and time that the project modified.

SIGNATURE
Type:\t\t string
Descrip.:\t A project based unique ID."

  (projectIDE-record-modify-time (gethash signature projectIDE-runtime-record)))

(defun projectIDE-set-project-name (signature name)
  "Set the project NAME of given SIGNATURE in projectIDE-runtime-record.

NAME
Type:\t\t string
Descrip.:\t project name

SIGNATURE
Type:\t\t string
Descrip.:\t A project based unique ID."
  
  (setf (projectIDE-record-name (gethash signature projectIDE-runtime-record)) name))

(defun projectIDE-set-project-path (signature path)
  "Set the project PATH of given SIGNATURE in projectIDE-runtime-record.

NAME
Type:\t\t string
Descrip.:\t project path

SIGNATURE
Type:\t\t string
Descrip.:\t A project based unique ID."

  (setf (projectIDE-record-path (gethash signature projectIDE-runtime-record)) path))

(defun projectIDE-set-project-modify-time (signature)
  "Set the project modify time given by SIGNATURE to current time.

SIGNATURE
Type:\t\t string
Descrip.:\t A project based unique ID."

  (setf (projectIDE-record-modify-time (gethash signature projectIDE-runtime-record)) (current-time)))
;; record object ends
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; cache and project object
(cl-defstruct projectIDE-project
  signature                                    ;; string         eg. "173874102"
  name                                         ;; string         eg. "project1"
  (exclude projectIDE-default-exclude)         ;; string-list    eg. ("*.git" ".projectIDE")
  (whitelist projectIDE-default-whitelist)     ;; string-list    eg. ("*.git" ".projectIDE")
  (cachemode projectIDE-default-cachemode)
  module
  reserve-field-1
  reserve-field-2
  reserve-field-3
  )

(cl-defstruct projectIDE-cache
  project                                      ;; project-project object
  config-update-time                           ;;
  exclude
  whitelist
  file-cache
  opened-buffer                                ;; string in terms of file path
  reserve-field-1
  reserve-field-2
  reserve-field-3
  )

;;; Getter and setter function
;; project object doesn't provide setter function
;; because it can only create by projectIDE-parse-config
;;fff (defun projectIDE-get-cache (signature))
;;fff (defun projectIDE-get-config-update-time (signature))
;;fff (defun projectIDE-get-project-exclude (signature))
;;fff (defun projectIDE-get-project-whitelist (signature))
;;fff (defun projectIDE-get-cache-exclude (signature))
;;fff (defun projectIDE-get-cache-whitelist (signature))
;;fff (defun projectIDE-get-cache-mode (signature))
;;fff (defun projectIDE-get-file-cache (signature))
;;fff (defun projectIDE-get-opened-buffer (signature))

(defun projectIDE-get-cache (signature)
  "Get a projectIDE-cache object of give SIGNATURE in projectIDE-runtime-cache.
It can also use to test whether project is in projectIDE-runtime-cache.

Return
Type:\t\t projectIDE-cache object or nil
Descrip.:\t The cache object of given signature or nil if not found.

SIGNATURE
Type:\t\t string
Descrip.:\t A project based unique ID."

  (gethash signature projectIDE-runtime-cache))

(defun projectIDE-get-config-update-time (signature)
  "Get the project config file update time in cache by given SIGNATURE.
Return nil if \".projectIDE\" no longer exists at path.

Return
Type:\t\t emacs time
Descrip.:\t\t Time of last update time of config file in projectIDE-runtime-cache.

SIGNATURE
Type:\t\t string
Descrip.:\t A project based unique ID."
  (projectIDE-cache-config-update-time (gethash signature projectIDE-runtime-cache)))

(defun projectIDE-get-project-exclude (signature)
  "Get the list of exclude from project object with given SIGNATURE in cache.

Return
Type:\t\t list of string
Descrip.:\t A list of exculding regexp

SIGNATURE
Type:\t\t string
Descrip.:\t A project based unique ID."
  
  (projectIDE-project-exclude
   (projectIDE-cache-project
    (gethash signature projectIDE-runtime-cache))))

(defun projectIDE-get-project-whitelist (signature)
  "Get the list of allowed from project object with given SIGNATURE in cache.

Return
Type:\t\t list of string
Descrip.:\t A list of whitelist regexp

SIGNATURE
Type:\t\t string
Descrip.:\t A project based unique ID."
  
  (projectIDE-project-whitelist
   (projectIDE-cache-project
    (gethash signature projectIDE-runtime-cache))))

(defun projectIDE-get-cache-exclude (signature)
  "Get the list of exclude form cache with given SIGNATURE.

Return
Type:\t\t list of string
Descrip.:\t A list of exculding regexp

SIGNATURE
Type:\t\t string
Descrip.:\t A project based unique ID."
  
  (projectIDE-cache-exclude (gethash signature projectIDE-runtime-cache)))

(defun projectIDE-get-cache-whitelist (signature)
  "Get the list of allowed form cache with given SIGNATURE.

Return
Type:\t\t list of string
Descrip.:\t A list of whitelist regexp

SIGNATURE
Type:\t\t string
Descrip.:\t A project based unique ID."
  
  (projectIDE-cache-whitelist (gethash signature projectIDE-runtime-cache)))

(defun projectIDE-get-cache-mode (signature)
  )

(defun projectIDE-get-file-cache (signature)
  "Get the file cache hash table from cache with given SIGNATURE.

Return
Type:\t\t hashtbale
Descrip.:\t A hashtable of file cache maintained by fdex

SIGNATURE
Type:\t\t string
Descrip.:\t A project based unique ID."
  
  (projectIDE-cache-file-cache (gethash signature projectIDE-runtime-cache)))

(defun projectIDE-get-opened-buffer (signature)
  "Get the opened buffer from cache with given SIGNATURE.

Return
Type:\t\t list of string
Descrip.:\t A list of opened buffer in terms of file path.

SIGNATURE
Type:\t\t string
Descrip.:\t A project based unique ID."

  (projectIDE-cache-opened-buffer (gethash signature projectIDE-runtime-cache)))
;; cache and project object ends
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~










(defun projectIDE-get-signature-from-buffer-trace (&optional buffer)
"Return the signature of project of given BUFFER.
If BUFFER is not provided, current buffer is used.

Return
Type:\t\t string
Descrip.:\t Signature of project from buffer trace

BUFFER
Type:\t\t Emacs buffer
Descrip.:\t If buffer is not provided, current buffer is used."
  (gethash (or buffer (current-buffer)) projectIDE-buffer-trace))


;;; Setter function

(defun projectIDE-set-project-in-cache (project signature)
"Set the PROJECT object in projectIDE-runtime-cache with given SIGNATURE.
Update the config update time as well.

PROJECT
Type:\t\t projectIDE-project object
Descrip.:\t Project object to be set in cache.

SIGNATURE
Type:\t\t string
Descrip.:\t A project based unique ID."

(let ((cache (gethash signature projectIDE-runtime-cache)))
    (setf (projectIDE-cache-project cache) project)
    (setf (projectIDE-cache-config-update-time cache) (current-time))))


(defun trim-string (string)
  "Return trimmed leading and tailing whitespace from STRING.
If the return string is a null string, return nil instead.

Return
Type:\t\t string or nil
Descrip.:\t Trimmed string or nil if blank string.

STRING
Type:\t\t string
Descrip.:\t String to be trimmed."
  (let ((return-string
         (replace-regexp-in-string (rx (or (: bos (* (any " \t\n")))
                                           (: (* (any " \t\n")) eos)))
                                   ""
                                   string)))
    ;; Return value
    (if (string= return-string "")
        nil
      return-string)))

(defun projectIDE-add-to-list (list element)
  "Implements `add-to-list'(LIST ELEMENT) with `cl-pushnew'.
Adds ELEMENT to  LIST if it isn't there yet.
It handles all situation when LIST or ELEMENT is nil.
This function does not modify the original list.
Instead, it returns a new list.
In addition, it accepts non-symbol LIST.

This function is expensive in term of efficency.
Avoid to use in heavy loop.

Return
Type:\t\t list
Descrip.:\t New list with ELEMENT add to LIST

LIST
Type:\t\t list of any type
Descrip.:\t List to be checked and appended to.

ELEMENT
Type:\t\t same type of LIST element
Descrip.:\t Add to LIST if ELEMENT isn't there yet."
  (let ((newlist (copy-tree list)))
    (if newlist
        (when element
          (setq newlist (cl-pushnew element newlist :test 'equal)))
      (when element
        (setq newlist (list element))))
    newlist))

(defun projectIDE-append (list1 list2)
  "Return a combined list of LIST1 and LIST2 and prevent duplication.
It accepts non-symbol LIST.

This function is expensive in term of efficency.
Avoid to use in heavy loop.

Return
Type:\t\t list
Descrip.:\t Combined list of LIST1 and LIST2.

LIST1/LIST2
Type:\t list of any type
Descrip.:\t List to be combined."
  (let (newlist)
    (setq newlist (append list1 list2))
    (cl-remove-duplicates newlist :test 'equal)))

(provide 'projectIDE-header)

