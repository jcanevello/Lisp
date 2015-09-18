(setq lista '(1 (2 3 4) 5 6 7 (8 9)))
(defun sh (l) 
(if(null (car l)) 0
   (
   if (atom (car l))
     (+ (car l) (sh (cdr l)))
     (sh (cdr l)))
))