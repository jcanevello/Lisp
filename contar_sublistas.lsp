(setq l2 '(1 2 (3 4 (5 6 (14 15 16))) 8 9 (10 11 (12 13))))

(defun contar_sublistas (l)
( if (null (car l)) 0
  (if (listp (car l))
      (+ 1 (+ (contar_sublistas (car l))  (contar_sublistas (cdr l))))
    (contar_sublistas (cdr l)))
))