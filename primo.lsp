(defun primo (n div) 
( cond ((= n 1) T)
       ((= div 1) T)
       ((= (mod n div) 0) NIL)
       (t (primo n (- div 1)))
))

(defun is_prime (n) 
(cond ((primo n (- n 1)) (list 'Es-primo))
       (t (list 'no-es-primo))
))

(setq l '(1 2 3 (3 4 (5 6)) 7 8 (9 10 (11 12 13 14))))