(projectIDE-load-module 'testmodule)
(projectIDE-load-module 'testmodule2)

(projectIDE-activate-module 'testmodule)
(projectIDE-activate-module 'testmodule2)

(projectIDE-deactivate-module 'testmodule)
(projectIDE-deactivate-module 'testmodule2)

(projectIDE-unload-module 'testmodule)
(projectIDE-unload-module 'testmodule2)

(projectIDE-print-variable projectIDE-runtime-packages)
(projectIDE-print-variable projectIDE-runtime-functions)

(progn "Use this to clean up this page. Just go to the end of this progn and run 'eval-last-sexp'"
       (goto-char (point-min)) (let (kill-ring) (comment-kill (count-lines (point-min) (point-max)))) (save-excursion (save-restriction (widen) (goto-char (point-max)) (delete-blank-lines) (let ((trailnewlines (abs (skip-chars-backward "\n\t")))) (if (> trailnewlines 0) (progn (delete-char trailnewlines)))))))
