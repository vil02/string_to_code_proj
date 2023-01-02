(defun fun_b ()
  (format T "~c" #\{)
  (format T "~c" #\%))

(defun fun_a ()
  (fun_b)
  (format T "~c" #\Newline)
  (fun_b))

(fun_a)
