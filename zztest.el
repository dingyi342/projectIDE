(require 'cl-lib)

(cl-defmacro zz-pro (name &key dir)
  `(defun ,(intern (concat "zz-" (downcase (symbol-name name)))) ()
     "This is prototype function."
     (interactive)
     (zz ,name
         :dir ,dir)
     (call-interactively  (quote ,(intern (concat "zz-" (downcase (symbol-name name))))))))

(cl-defmacro zz (name &key dir)
  (let* ((letname (downcase (symbol-name name)))
         (letdir dir))
  `(defun ,(intern (concat "zz-" letname)) ()
     "Great! This is real function."
     (interactive)
     (message "I am calleddddddddddddddddddddddddddddddd!")
     (message ,letdir))))

(provide 'zztest)
