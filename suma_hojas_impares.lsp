(defun shi (l)
( if (null (car l)) 0 
  (if (atom (car l))
      (if (= (mod (car l) 2) 1)
       (+ (car l) (shi (cdr l)))
       (shi (cdr l)))
   (shi (cdr l))
   )
))