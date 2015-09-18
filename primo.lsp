(defun primo (n div) 
( cond ((= n 1) T)  
       ((= div 1) T)
       ((= (mod n div) 0) NIL)
       (t (primo n (- div 1)))
))

(defun is_prime (n) 
(primo n (- n 1)
))

(setq l '(1 2 3 (3 4 (5 6)) 7 8 (9 10 (11 12 13 14))))

(defun aplanar (l)
( cond ((null (first l)) NIL)
       ((atom (first l)) (cons (first l) (aplanar (rest l))) )
       (t (append (aplanar (first l)) (aplanar (rest l))))
))  

;Funciona para listas simples
(defun get-primos (l)
( cond ((null (first l)) NIL)
       ((atom (first l)) ( cond ((is_prime (first l)) (cons (first l) (get-primos (rest l))))
                                (t (get-primos (rest l)))
                          ))
       (t (get-primos (rest l)))
))

;Funciona para listas compuestas
(defun get_primos (l)
( cond ((null (first l)) NIL)
       ((atom (first l)) (cond ((is_prime (first l)) (cons (first l) (get_primos (rest l))))
                               (t (get_primos (rest l)))
                          ))
       (t (append (get_primos (first l)) (get_primos (rest l))))
))