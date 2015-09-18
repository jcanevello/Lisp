(setq l2 '(1 2 3 4 5 6 7))

(defun sumapar (list)
( cond ((null list) 0 )
       (t (
           if (= (mod (car list) 2) 1) 
              (+ (car list) (sumapar (cdr list)))
              (sumapar (cdr list))
           ))
)
)