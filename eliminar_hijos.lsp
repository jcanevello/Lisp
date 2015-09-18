(setq l '(1 (2 3 4) 5 6 7 (8 9)))
(defun eh (l)
( cond ((null (car l)) NIL)
       (t (
           cond ((listp (car l)) (cons (car l) (eh (cdr l)))  )
                (t (eh (cdr l)))
           ))
))