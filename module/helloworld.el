(projectIDE-defun helloworld-say-hello ()
                  "Say hello world."
                  (interactive)
                  (message "Hello World."))

(projectIDE-set-key (kbd "C-c p") 'helloworld-say-hello)

(provide 'helloworld)
