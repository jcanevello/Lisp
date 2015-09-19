(defun eleva (n m)
( cond ((zerop m) 1)
       (t (* n (eleva n (- m 1))))
))