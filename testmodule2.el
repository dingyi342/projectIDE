(projectIDE-defun testmodule2-testfunc ()
                  "It is a test function for testing module."
                  (interactive)
                  (message "Calling testmodule-testfunc"))

(projectIDE-cl-defmacro testmodule2-++ (arg &key step)
                        "A ++ testing macro for cl-defmacro"
                        `(setq ,arg (+ ,arg (or ,step 1))))

(provide 'testmodule2)
