(defun spnh (l)
( if (null (car l)) 0
  (if (listp (car l))
      (+ (sph (car l)) (spnh (cdr l)))
    (spnh (cdr l)))
))