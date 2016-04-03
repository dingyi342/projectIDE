
(require 'bytecomp)
(require 'projectAPI)

(defun ecompile-all ()

  (byte-recompile-directory (projectAPI-project-root) 0))

(defun ecompile-files (&rest files)
  
  
  (with-current-buffer (get-buffer-create byte-compile-log-buffer)
    (setq default-directory (projectAPI-project-root (projectAPI-active-project)))
    (unless (eq major-mode 'compilation-mode)
      (compilation-mode))
    
    (let ((skip-count 0)
          (fail-count 0)
          (file-count 0))

      (displaying-byte-compile-warnings
       (dolist (file files)
         (when (and (string-match emacs-lisp-file-regexp file)
                    ;; The next 2 tests avoid compiling lock files
                    (file-readable-p file)
                    (not (string-match "\\`\\.#" file))
                    (not (auto-save-file-name-p file))
                    (not (string-equal dir-locals-file
                                       (file-name-nondirectory file))))
           (cl-incf
            (pcase (byte-recompile-file file nil 0)
              (`no-byte-compile skip-count)
              (`t file-count)
              (_ fail-count)))))
       (message "Done (Total %d of %d file%s compiled%s%s)"
                file-count (length files)
                (if (= file-count 1) "" "s")
                (if (> fail-count 0) (format ", %d failed" fail-count) "")
                (if (> skip-count 0) (format ", %d skipped" skip-count) ""))))))

(provide 'ecompile)
;;; ecompile.el ends here
