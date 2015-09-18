(defun potencia (m n) 
(
 cond ((= n 0) 1 )
       (t (* m (potencia m (- n 1))))
)
)