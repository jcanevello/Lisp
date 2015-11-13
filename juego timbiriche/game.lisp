;//////////////////////////////////////////////////////////////////////////
;cargamos el archivo donde se encuentra el algoritmo minimax
(load "~/Descargas/algoritmo.lisp")

;//////////////////////////////////////////////////////////////////////////
;Funcion que inicializa las variables globales que va a utilizar
; el juego cuadraditos (El timbiriche)
(defun inicializar-juego()
  (setq
    *Tablero* '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)   ; Lista Tablero que almacena la Matriz 5*5 del juego.
    *lineaHh* 'h ;variable que pinta la linea horizontal del humano
    *lineaHm* 'm  ; Variable que pinta la linea horizontal del ordenador
    *lineaVh* 'h  ; Variable que pinta la linea vertical del humano
    *lineaVm* 'm  ; Variable que pinta la linea vertical del ordenador
    *Turno*  -1 ;1  ; Variable que almacena el turno del juego
    *Humano* 1      ;Variable que identifica al humano como 1
    *Ordenador* -1  ;Variable que identifica al ordenador como -1
    *LineasDisp* '(0 1 2 3 4 5 6 7 8 9 10 11)   ;Lista que almacena las lineas posibles donde se pueden trazar
    *NroHumano* 0    ;Variable que almacena los cuadrados hechos por el Humano
    *NroOrdenador* 0   ;Variable que almacena los cuadrados hechos por el Ordenador
    *Cuad* ' ((0 2 3 5) (1 3 4 6) (5 7 8 10) (6 8 9 11)) ; Lista que almacena sublistas de las posiciones que forman un cuadrado en el tablero
   )	
)

;Agregamos en el tablero el elemento en la posicion querida.
(defun editar-tablero (posicion elemento)
  (setq tab *tablero*); Tablero auxiliar (tab) 
  (setq listaux '())  ;lista auxiliar
  (setq cnt 0)        ;inicializamos el contador
  (if (< posicion (length tab))
      (progn
         ;aqui vamos a empezar a recorrer la lista
         (dolist (x tab)
            (if (or (< cnt posicion) (> cnt posicion))
                (progn
                  (setq listaux (append listaux (list x)))  ;almacenamos los valores actuales del tablero si es que no se ha llegado a la posicion deseada para cambiar
                )
            )
            (if (= cnt posicion) 
                (progn
                    (setq listaux (append listaux (list elemento)))  ;Sobrescribimos en la posicion dada el nuevo elemento y luego lo almacenamos a Lista aux
                 )
            )
            (setq cnt (+ cnt 1)) ; Aumentados el contador para que recorra todas las posiciones del Tablero
         )
         (setq *tablero* listaux) ; Asignamos la Lista Auxiliar a la variable global *tablero*
      )
      ;si no cumple
      (print "Posicion no permitida")
  )
)

;funcion para probar si un elemento pertenece a una lista
(defun probar (elemento lista)
  (if (equal nil (member elemento lista))
      (setq valor nil)   ;no esta
      (setq valor t)     ;si esta
  )
)




(defun imprimir-tablero ()
  (format t "~%")
  
  (format t "| ")
  (format t "~A" (convertir-a-linea 0 (nth 0 *tablero*)) )
  (format t "|")
  (format t "~A" (convertir-a-linea 1  (nth 1 *tablero*)) )
  (format t "| ")
  
  (format t "~& ")  (format t "~A" (convertir-a-linea 2  (nth 2 *tablero*)) )
  (format t "   ")
  (format t "~A" (convertir-a-linea  3 (nth 3 *tablero*)) )
  (format t "   ")
  (format t "~A" (convertir-a-linea  4 (nth 4 *tablero*)) )
  (format t "|")
  (format t "~A" (convertir-a-linea  5 (nth 5 *tablero*)) )
  (format t "|")
  (format t "~A" (convertir-a-linea  6 (nth 6 *tablero*)) )
  (format t "|")

  (format t "~& ")
  (format t "~A" (convertir-a-linea  7 (nth 7 *tablero*)) )
  (format t "   ")
  (format t "~A" (convertir-a-linea  8 (nth 8 *tablero*)) )
  (format t "   ")
  (format t "~A" (convertir-a-linea  9 (nth 9 *tablero*)) )

  (format t "|")
  (format t "~A" (convertir-a-linea  10 (nth 10 *tablero*)) )

  (format t "|")
  (format t "~A" (convertir-a-linea  11 (nth 11 *tablero*)) )
  (format t "|")

  (format t "~%")
)




      

;funcion para quitar linea de la lista de disponibles
(defun eliminar-disp (linea)
  (setq disponible *LineasDisp*)
  (setq listaux '())  ;lista auxiliar
  (setq cnt 0)        ;inicializamos el contador
  (dolist (x disponible)
  (if (not(equal x linea))
            (progn
      (setq listaux (append listaux (list x)))  ;Almacena en la lista auxiliar todo los elementos diferentes de la linea a eliminar
     
      )
  )
  setq cnt (+ cnt 1))
  (setq *LineasDisp* listaux)  ;Actualizamos la Variable global de *LineasDisp*
)



;funcion para identificar el cuadrado real a pintar
(defun llenarCuad (numCuad)
  (cond ((equal numCuad 0) 12)
        ((equal numCuad 1) 13)
        ((equal numCuad 2) 14)
        ((equal numCuad 3) 15)
  )
)

;función para pintar cuadraditos
(defun pintar (pos tur)
  (editar-tablero pos tur)
  
)

;funcion para verificar si se llenó algún cuadrado
(defun verificarCuad (linea turno)
  (setq bandera nil) ;Variable bandera que nos sirve para ver si Continua con el turno o no.
  (dotimes (i 4) ;Verifica por los 4 cuadrados del tablero
     (setq cnt 0)
     (setq cuadradito (nth i *Cuad*)) ;Almacenamos en cuadradito las posiciones de un cuadrado posible
     (if (probar linea cuadradito) ; Verificamos que si la linea a trazar pertenece a esa lista de cuadradito
         (progn
              (dolist (x cuadradito); recorremos la lista cuadradito
                  (if (not(equal (nth x *Tablero*) 0))
                      (setq cnt (+ 1 cnt)) ; Aumentamos el contador por cada vez que este trazada las posiciones de la lista cuadradito
                  )
               )
               (if (= cnt 4) ; Si el contador llega a 4, entonces tenemos que pintarlo
                   (progn
                       (pintar (llenarCuad i) turno)
                       (setq bandera t)
                       (if (= turno 1)
                         (setq *NroHumano* (+ 1 *NroHumano*))   ;Aumentamos el contador de cuadrados hechos por el humano
                         (setq *NroOrdenador* (+ 1 *NroOrdenador*)) ;Aumentamos el contador de cuadrados hechos por el ordenador
                       )
                   )
               )
         )     
     )
  )
  (if (equal bandera nil) (setq *turno* (* -1 *turno*))) ;si no cerramos un cuadrado cambiamos de turno
)

;funcion para jugar


(defun comenzar()
(inicializar-juego)
;(jugar-humano)
(jugar-ordenador) 
                          

)





(defun jugar-humano () 
 (print "ingresa  ")
(setq linea (read))
(setq disp *LineasDisp*) 
 
  (if (probar linea disp) ;Verificamos si la linea a trazar esta disponible
      (progn 
           (editar-tablero linea *humano*)  ; Editamos el elemento en el tablero de acuerdo a su posicion
           (eliminar-disp linea)  ; Eliminamos esa linea de la lista de Disponibles
           (verificarCuad linea *turno*) ; Verificamos si con ese trazado se ha formado un cuadrado para el humano
      )
  )
     (print "JUEGA HUMANO") 
     (imprimir-tablero)

    (cond ((=  (length *LineasDisp*)0 ) (format t  "~s" (estado-juego)))
          ((= -1 *turno*) (jugar-ordenador))
          (t (jugar-humano))
    )




)


(defun estado-juego ()
  (cond ((<  *NroOrdenador* *NroHumano* ) (format t "~&Gana usted"))
        ((>  *NroOrdenador* *NroHumano*) (format t "~&Gana pc"))
        (t (format t "~&Empate"))
  )
  (format t "~%")
)



(defun jugar-ordenador ()

  (if (> (length *LineasDisp*) 0)

    (progn 
		;llamamos a algoritmo minimax, que nos devuelve linea a escoger
		(setq lineaC (algoritmo_minimax *Tablero*))

       (setq posreal  lineaC) ;identificamos la posicion real de acuerdo al tablero.
       (editar-tablero posreal *Ordenador*)  ; Editamos el elemento en el tablero de acuerdo a su posicion
       (eliminar-disp lineaC)   ; Eliminamos esa linea de la lista de Disponibles
       (verificarCuad posreal *turno*); Verificamos si con ese trazado se ha formado un cuadrado para el ordenador
       (if (= *turno* -1)
        (jugar-ordenador)
        )

       (print "JUEGA ORDENADOR") 
       (imprimir-tablero)
     )
  )
   
   (cond ((=  (length *LineasDisp*)0 ) (format t  "~s" (estado-juego)))
          ((= 1 *turno*) (jugar-humano))
          
    )



)

(defun convertir-a-linea (pos elemento )
(cond 

((and (= 0 pos ) (= 1 elemento ) ) *lineaHh*)
((and (= 0 pos ) (= -1 elemento  ))*lineaHm*)
((and (= 1 pos ) (= 1 elemento ))  *lineaHh*)
((and (= 1 pos ) (= -1 elemento )) *lineaHm* )    
((and (= 2 pos ) (= 1 elemento  )) *lineaVh*)
((and (= 2 pos ) (= -1 elemento )) *lineaVm* )
((and (= 3 pos ) (= 1 elemento )) *lineaVh*)
((and (= 3 pos ) (= -1 elemento ))*lineaVm* )
((and (= 4 pos ) (= 1 elemento )) *lineaVh*)
((and (= 4 pos ) (= -1 elemento  ))*lineaVm*)
((and (= 5 pos ) (= 1 elemento ))*lineaHh* )
((and (= 5 pos ) (= -1 elemento)) *lineaHm* )
((and (= 6 pos ) (= 1 elemento )) *lineaHh*)
((and (= 6 pos ) (= -1 elemento )) *lineaHm*    )
((and (= 7 pos ) (= 1 elemento ))  *lineaVh*      )
((and (= 7 pos ) (= -1 elemento )) *lineaVm*     )
((and (= 8 pos ) (= 1 elemento  ))  *lineaVh*     )
((and (= 8 pos ) (= -1 elemento ))  *lineaVm*      )
((and (= 9 pos ) (= 1 elemento ))   *lineaVh*    )
((and (= 9 pos ) (= -1 elemento ))  *lineaVm*   )
((and (= 10 pos ) (= 1 elemento ))  *lineaHh*  )
((and (= 10 pos ) (= -1 elemento )) *lineaHm*    )
((and (= 11 pos ) (= 1 elemento ))  *lineaHh*    )
((and (= 11 pos ) (= -1 elemento )) *lineaHm*  )
                                                                                               
(t " ")
)

)
