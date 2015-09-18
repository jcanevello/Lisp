(defun contimp (l)
( cond ((null l) 0)
       (t (
           if (= (mod (car l) 2) 0)
              (+ 1 (contimp (cdr l)))
              (contimp (cdr l))
))
))