;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(projectIDE-defun testfunc (number)
                  (interactive)
                  (message "The number is %s." (int-to-string number)))

(projectIDE-realize-defun 'testfunc)
(testfunc 61)

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(projectIDE-cl-defun testfunc2 (number &key string number2)
                     (interactive)
                     (message "The number is %s and %s.\n%s"
                              (int-to-string number)
                              (or (and number2 (int-to-string number2)) "nil")
                              (or string "There is no additional string.")))

(projectIDE-realize-cl-defun 'testfunc2)
(testfunc2 16
           :number2 78
           :string "Hello")
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(projectIDE-defmacro -- (arg)
                     "a testing macro"
                     `(setq ,arg (- ,arg 1)))

(projectIDE-realize-defmacro '--)
(setq a 100)
(-- a)
a
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(projectIDE-cl-defmacro ++ (arg &key step)
                        "A ++ testing macro for cl-defmacro"
                        `(setq ,arg (+ ,arg (or ,step 1))))

(projectIDE-realize-cl-defmacro '++)

(setq b 100)
(++ b)
(++ b :step 3)
b
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
