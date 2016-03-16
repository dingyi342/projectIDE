(projectIDE-print-variable projectIDE-runtime-Btrace)
(projectIDE-print-variable projectIDE-runtime-cache)
(projectIDE-print-variable projectIDE-runtime-record)
(projectIDE-print-variable projectIDE-runtime-packages)
(projectIDE-print-variable projectIDE-runtime-functions)

(progn "Use this to print the return value of a function."(goto-char (point-max))(insert "\n\n")(let ((beg (point)))(save-excursion (insert (pp
                                                                                                                                             
(projectIDE-get-project-list)
                                                                                                                                             
 ))(comment-region beg (point)))))

(progn "Use this to clean up this page. Just go to the end of this progn and run 'eval-last-sexp'"
       (goto-char (point-min)) (let (kill-ring) (comment-kill (count-lines (point-min) (point-max)))) (save-excursion (save-restriction (widen) (goto-char (point-max)) (delete-blank-lines) (let ((trailnewlines (abs (skip-chars-backward "\n\t")))) (if (> trailnewlines 0) (progn (delete-char trailnewlines)))))))
