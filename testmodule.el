(projectIDE-defun testmodule-testfunc ()
                  "It is a test function for testing module."
                  (interactive)
                  (message "Calling testmodule-testfunc"))

(projectIDE-cl-defmacro testmodule-++ (arg &key step)
                        "A ++ testing macro for cl-defmacro"
                        `(setq ,arg (+ ,arg (or ,step 1))))

(provide 'testmodule)
