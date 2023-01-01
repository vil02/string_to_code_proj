(defun fun_b ())

(defun fun_a ()
  (fun_b)
  (format T "~c" #\C))

(fun_a)
