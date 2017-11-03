;; Fichero "practica1.rkt"
;; Ejercicios de la práctica 1


;;;;
;; Ejercicio 7
;;;;

;;
;; Nombre: area-rombo
;; Objetivo: calcular el área de un rombo dadas sus diagonales
;; Parámetros:
;;   D: diagonal mayor
;;   d: diagonal menor
;; Resultados:
;;   Área del rombo definido por los parámetros recibidos
;; Descripción:
;;   Se aplica la fórmula para el área de un rombo
;;
(define (area-rombo1 d-mayor d-menor)
  (/ (* d-mayor d-menor) 2.)
  )

;;
;; Nombre: area-trapecio
;; Objetivo: calcular el área de un trapecio
;; Parámetros:
;;   B: base mayor
;;   b: base menor
;;   h: altura
;; Resultados:
;;   Área del trapecio según los parámetros recibidos
;; Descripción:
;;   Se aplica la fórmula para el área de un trapecio
;;
(define (area-trapecio1 b-mayor b-menor h)
  (* (/ (+ b-mayor b-menor) 2) h)
  )


;;;;
;; Ejercicio 8
;;;;

;;
;; Nombre: celsius-a-fahrenheit
;; Objetivo: convierte grados Celsius a Fahrenheit
;; Parámetros:
;;   celsius: temperatura en grados celsius
;; Resultado:
;;   Temperatura convertida a grados celsius
;; Descripción:
;;   Se multiplica por el factor de conversión y se suma 32
;;
(define (celsius-a-fahrenheit celsius)
  (+ (* celsius 1.8) 32)
  )

;;;;
;; Ejercicio 9
;;;;

;;
;; Nombre: volumen-esfera
;; Objetivo: calcula el volumen de una esfera dado su radio
;; Parámetros:
;;   r: radio de la esfera
;; Resultado:
;;   Volumen de la esfera con radio r
;; Descripción:
;;   Aplica la fórmula para el volumen de una esfera
;;
(define (volumen-esfera r)
  (/ (* 4 pi r r) 3)
  )

;;
;; Nombre: superficie-esfera
;; Objetivo: calcula la superficie de una esfera dado su radio
;; Parámetros:
;;   r: radio de la esfera
;; Resultado:
;;   Superficie de la esfera de radio r
;; Descripción:
;;   Aplica la fórmula para la superficie de una esfera
;;
(define (superficie-esfera r)
  (* 4 pi r r)
  )

;;
;; Nombre: altura-piramide
;; Objetivo: calcula la altura de una pirámide cuadrangular
;; Parámetros:
;;   l: lado de la base
;; Resultado:
;;   Altura de la pirámide cuadrangular de lado l
;; Descripción:
;;   Aplica la fórmula para la altura de una pirámide cuadrangular (teorema Pitágoras)
;;
(define (altura-piramide l)
  (/ l (sqrt 2))
  )
  
;;
;; Nombre: volumen-piramide
;; Objetivo: calcula el volumen de una pirámide cuadrangular
;; Parámetros:
;;   l: lado de la base
;; Resultado:
;;   Volumen de la pirámide de lado l
;; Descripción:
;;   Aplica la fórmula para el volumen de una pirámide
;;
(define (volumen-piramide l)
  (/ (* (sqrt 2) l l l) 6)
  )

;;
;; Nombre: area-piramide
;; Objetivo: calcula el área total de una pirámide cuadrangular
;; Parámetros:
;;  l: lado de la base
;; Resultado:
;;   Área de la pirámide de lado l
;; Descripción
;;   Aplica la fórmula para el área de una pirámide
;;
(define (area-piramide l)
  (* (+ 1 (sqrt 3)) l l)
  )
  
;;;;
;; Ejercicio 10
;;;;

;;
;; Nombre: distancia-euclidiana
;; Objetivo: calcula la distancia eunclídea entre dos puntos
;; Parámetros:
;;   x1: coordenada x del primer punto
;;   y1: coordenada y del primer punto
;;   x2: coordenada x del segundo punto
;;   y2: coordenada y del segundo punto
;; Resultado:
;;   Distancia euclidiana entre los dos puntos recibidos
;; Descripción:
;;   Aplica la fórmuala para la distancia euclidiana entre puntos
;;
(define (distancia-euclidiana x1 y1 x2 y2)
  (sqrt (+ (expt (- x2 x1) 2) (expt (- y2 y1) 2)))
  )

;;
;; Nombre: distancia-manhattan
;; Objetivo: calcula la distancia de Manhattan entre dos puntos
;; Parámetros:
;;   x1: coordenada x del primer punto
;;   y1: coordenada y del primer punto
;;   x2: coordenada x del segundo punto
;;   y2: coordenada y del segundo punto
;; Resultado:
;;   Distancia de Manhattan entre los dos puntos recibidos
;; Descripción:
;;   Aplica la fórmula para la distancia de Manhattan entre dos puntos
;;
(define (distancia-manhattan x1 y1 x2 y2)
  (+ (abs (- x2 x1)) (abs (- y2 y1)))
  )

;;
;; Nombre: distancia-ajedrez
;; Objetivo: calcula la distancia de ajedrez entre dos puntos
;; Parámetros:
;;  x1: coordenada x del primer punto
;;  y1: coordenada y del primer punto
;;  x2: coordenada x del segundo punto
;;  y2: coordenada y del segundo punto
;; Resultado:
;;   Distancia de ajedrez entre los dos puntos recibidos
;; Descripción:
;;   Aplica la fórmula para la distancia de ajedrez entre dos puntos
;;
(define (distancia-ajedrez x1 y1 x2 y2)
  (max (abs (- x2 x1)) (abs (- y2 y1)))
  )
  
;;;;
;; Ejercicio 11
;;;;

;;
;; Nombre: area-rombo
;; Objetivo: calcula el área de un rombo dados sus vértices
;; Parámetros:
;;  xi: Coordenada x del punto número i
;;  yi: Coordenada y del punto número i
;;  Nota: Las coordenadas (x1,y1) y (x2,y2) deben formar una de las diagonales
;;        Las coordenadas (x3,y3) y (x4,y4) deben formar la otra diagonal
;; Resultado:
;;   Área del rombo definido por los puntos recibidos
;; Descripción:
;;   Halla la longitud de los lados y aplica la fórmula de área de un rombo
;; Funciones a las que llama: distancia-euclidiana
;;
(define (area-rombo x1 y1 x2 y2 x3 y3 x4 y4)
  (let ((d1 (distancia-euclidiana x1 y1 x2 y2))
        (d2 (distancia-euclidiana x3 y3 x4 y4))
        )
    (/ (* d1 d2) 2.)
    )
  )

;; 
;; Nombre: area-triangulo
;; Objetivo:
;;   Calcula el área de un triágulo usando la fórmula de Herón(usa los vértices)
;; Parámetros:
;;   xi: Coordenada x del punto número i
;;   yi: Coordenada y del punto número i
;; Resultado:
;;   Área del triángulo definido por los puntos recibidos
;; Descripción:
;;   Obtiene el área aplicando la fórmula de Herón
;; Funciones a las que llama: distancia-euclidiana
;;
(define (area-triangulo x1 y1 x2 y2 x3 y3)
  (let* ((a (distancia-euclidiana x1 y1 x2 y2))
         (b (distancia-euclidiana x2 y2 x3 y3))
         (c (distancia-euclidiana x3 y3 x1 y1))
         (s (/ (+ a b c) 2.))
         )
    (sqrt (* s (- s a) (- s b) (- s c)))
    )
  )

;;;;
;; Ejercicio 12
;;;;

;; 
;; Nombre: distancia-punto-recta
;; Objetivo: calcula la distancia entre un punto y un recta
;; Parámetros:
;;   x: Coordenada x del punto
;;   y: Coordenada y del punto
;;   a: Coeficiente de la x para la ecuación de la recta
;;   b: Coeficiente de la y para la ecuación de la recta
;;   c: Coeficiente independiente
;; Resultado:
;;   Distancia entre el punto y la recta recibidos
;; Descripción:
;;   Para la recta se piden los coeficientes de su ecuación general:
;;     ax + by + c = 0
;;   Para la distancia de usa la fórmula para la distancia punto-recta
;;
(define (distancia-punto-recta x y a b c)
  (/
   (abs (+ (* a x) (* b y) c))
   (sqrt (+ (expt a 2) (expt b 2)))
   )
  )

;; 
;; Nombre: distancia-punto-recta2
;; Objetivo: calcula la distancia entre un punto y un recta
;; Parámetros:
;;   xi: Coordenada x del punto i
;;   yi: Coordenada y del punto i
;;   Nota: los puntos 1 y 2 forman la recta
;; Resultado:
;;   Distancia entre el punto y la recta recibidos
;; Descripción:
;;   Se hayan los coeficientes para la ecuación general de la recta
;;   y se llama a la función distancia-punto-recta
;; Funciones a las que llama: distancia-punto-recta
;;
(define (distancia-punto-recta2 x0 y0 x1 y1 x2 y2)
  (let* ((a (/ (- y2 y1) (- x2 x1))) ;; a = (y2-y1)/(x2-x1)
         (b -1)
         (c (- y1 (* a x1))) ;; c = y1-a*x1
         )
    (distancia-punto-recta x0 y0 a b c)
    )
  )

;;;;
;; Ejercicio 13
;;;;

;; 
;; Nombre: area-trapecio-let
;; Objetivo: calcula el área de un trapecio dados sus vértices
;; Parámetros:
;;   xi: Coordenada x del vértice i
;;   yi: Coordenada y del vértice i
;;   Nota: se espera que vértices 1 y 2 formen la base menor, y
;;         los vértices 3 y 4 formen la base mayor
;;              1 .___. 2
;;                /   \
;;             3 .-----. 4
;; Resultado:
;;   Área del trapecio definido por los vértices
;; Descripción:
;;   Se calcula la longitud de las bases y la altura, luego se aplica
;;   la fórmula del área de un trapecio
;; Funciones a las que se llama: distancia-euclidiana, distancia-punto-recta-2
;;
(define (area-trapecio-let x1 y1 x2 y2 x3 y3 x4 y4)
  (let ((b-menor (distancia-euclidiana x1 y1 x2 y2))
        (b-mayor (distancia-euclidiana x3 y3 x4 y4))
        (altura (distancia-punto-recta2 x1 y1 x3 y3 x4 y4))
        )
    (* (/ (+ b-mayor b-menor) 2) altura)
    )
  )

;;
;; Nombre: area-cuadrilatero-let
;; Objetivo: calcula el área de un cuadrilátero convexo
;; Parámetros:
;;   xi: Coordenada x del vértice i
;;   yi: Coordenada y del vértice i
;;   Nota: se espera que los vértices estén ordenados
;; Resultado:
;;   Área del cuadrilátero definido por los puntos recibidos
;; Descripción:
;;   Halla el área del cuadrilátero usando triangularización
;; Funciones a las que se llama: distancia-euclidiana, area-triangulo
;;
(define (area-cuadrilatero-let x1 y1 x2 y2 x3 y3 x4 y4)
  (let ((area-t1 (area-triangulo x1 y1 x2 y2 x3 y3))
        (area-t2 (area-triangulo x1 y1 x3 y3 x4 y4))
        )
    (+ area-t1 area-t2)
    )
  )
