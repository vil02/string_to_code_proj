(defun fun_0 ()
  (format T "~c" #\{)
  (format T "~c" #\%))

(defun fun_1 ()
  (fun_0)
  (format T "~c" #\Newline)
  (fun_0))

(fun_1)
