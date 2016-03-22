(defun projectIDE-reset ()
  (interactive)
  (projectIDE-terminate)
  (delete-directory projectIDE-database-path t t)
  (delete-file "/home/tom/.emacs.d/elpa/projectIDE/.projectIDE"))

(projectIDE-reset)
