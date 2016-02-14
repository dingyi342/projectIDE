(projectIDE-print-variable projectIDE-opened-project)
(projectIDE-print-variable projectIDE-buffer-trace)
(projectIDE-print-variable projectIDE-runtime-cache)
(projectIDE-print-variable projectIDE-runtime-record)

(progn "Use this to clean up this page. Just go to the end of this progn and run 'eval-last-sexp'"
       (goto-char (point-min)) (let (kill-ring) (comment-kill (count-lines (point-min) (point-max)))) (save-excursion (save-restriction (widen) (goto-char (point-max)) (delete-blank-lines) (let ((trailnewlines (abs (skip-chars-backward "\n\t")))) (if (> trailnewlines 0) (progn (delete-char trailnewlines)))))))
