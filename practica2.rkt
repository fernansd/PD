;;
;; Fichero: "practica2.rkt"
;; Descripción: Ejercicios de la práctica 2
;; Autor: Fernando Sánchez Delgado

;;;; PREGUNTAS
;;- Ejercicio 6
;; (posicion-circunferencia-recta 0 0 3 1 -1 4.24)
;;- Ejercicios 7 y 9
;;  Necesarios tantos ejemplos?

;;;;
;; Ejercicio 3
;;;;

;;
;; Nombre: redondear
;; Objetivo: redondea el número recibido
;; Parámetros:
;;   num: número a redondear
;; Resultados:
;;   Número redondeado
;; Descripción:
;;   Para una parte decimal d tal que 0 <= d < 0.5 se redondea
;;   hacia abajo, en caso contrario hacia arriba.
;;
(define (redondear num)
  (let* (;; Separa el número en parte entera y decimal
         (entera (truncate num))
         (decimal (- num entera)))
    ;; Comprueba el valor de la parte decimal
    (if (< decimal 0.5)
        entera ;; Redondea hacia abajo
        (+ entera 1) ;; Redondea hacia arriba
        )
    )
  )
;; Ejemplo:
;;(redondear 4.3)
;; = 4
;;(redondear 3.5)
;; = 4
;;;;

;;;;
;; Ejercicio 4
;;;;

;;
;; Nombre: letra-dni
;; Objetivo: obtiene la letra del dni dado el número
;; Parámetros:
;;   num: número de dni
;; Resultados:
;;   Letra asociada al número recibido
;; Descripción:
;;   Se calcula el resto de dividir el número entre 23
;;   y dependiendo del resto, se asocia una letra. Existe
;;   una tabla de asociación del resto con una letra
;;
(define (letra-dni num)
  ;; Obtiene el resto y compara con la tabla
  (case (remainder num 23)
    ((0) "T")
    ((1) "R")
    ((2) "W")
    ((3) "A")
    ((4) "G")
    ((5) "M")
    ((6) "Y")
    ((7) "F")
    ((8) "P")
    ((9) "D")
    ((10) "X")
    ((11) "B")
    ((12) "N")
    ((13) "J")
    ((14) "Z")
    ((15) "S")
    ((16) "Q")
    ((17) "V")
    ((18) "H")
    ((19) "L")
    ((20) "C")
    ((21) "K")
    ((22) "E")
    )
  )

;;;;
;; Ejercicio 5
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
;;   Aplica la fórmula para la distancia euclidiana entre puntos:
;;     distancia = sqrt((x2 - x1)^2 + (y2 - y1)^2)
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
;;   Aplica la fórmula para la distancia de Manhattan entre dos puntos:
;;     distancia = abs(x2 - x1) + abs(y2 - y1)
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
;;   Aplica la fórmula para la distancia de ajedrez entre dos puntos:
;;     distancia = max(abs(x2 - x1), abs(y2 - y1))
;;
(define (distancia-ajedrez x1 y1 x2 y2)
  (max (abs (- x2 x1)) (abs (- y2 y1)))
  )

;;
;; Nombre: posicion-punto
;; Objetivo: localizar dónde está un punto relativo a unas formas geométricas
;; Parámetros:
;;   x: coordenada x del punto
;;   y: coordenada y del punto
;; Resultados:
;;   Número asociado a la posición del número
;; Descripción:
;;   Dependiendo de la posición del punto, asigna un número:
;;    0: punto pertenece a alguna de las figuras geométricas
;;    1: punto dentro del rombo
;;    2: punto dentro del círculo y fuera del rombo
;;    3: punto dentro del cuadrado
;;    4: otro caso
;;   Usa las propiedades de cada una de las distancias para decidir
;;   como se posiciona respecto a cada figura geométrica.
;; Funciones a las que llama: distancia-euclidiana, distancia-manhattan,
;;   distancia-ajedrez
;;
(define (posicion-punto x y)
  (let (;; Calcula las distancias del punto al centro de las figuras (0,0)
        (euclidea (distancia-euclidiana 0 0 x y))
        (manhattan (distancia-manhattan 0 0 x y))
        (ajedrez (distancia-ajedrez 0 0 x y))
        )
    (cond
      ;; Pertenece a una figura
      ((or (= euclidea 1) (= manhattan 1) (= ajedrez 1)) 0)
      ;; Dentro del rombo
      ((< manhattan 1) 1)
      ;; Dentro de círculo pero fuera del rombo
      ((and (< euclidea 1) (> manhattan 1)) 2)
      ;; Dentro del cuadrado pero fuera del círculo
      ((< ajedrez 1) 3)
      (else 4)
      )
    )
  )
;; Ejemplos:
;;(posicion-punto 0 1)
;; = 0
;;(posicion-punto 0 0)
;; = 1
;;(posicion-punto 0.6 0.6)
;; = 2
;;(posicion-punto 0.8 0.8)
;; = 3
;;(posicion-punto 0 2)
;; = 4
;;;;

;;;;
;; Ejercicio 6
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
;;   Para la distancia de usa la fórmula para la distancia punto-recta:
;;     distancia = abs(a*x + b*y + c)/sqrt(a^2 + b^2)
;;
(define (distancia-punto-recta x y a b c)
  (/
   (abs (+ (* a x) (* b y) c))
   (sqrt (+ (expt a 2) (expt b 2)))
   )
  )

;;
;; Nombre: posicion-circunferencia-recta
;; Objetivo:
;;   Determina la posición relativa de una recta respecto una circunferencia
;; Parámetros:
;;   x1: coordenada x del primer punto
;;   y1: coordenada y del primer punto
;;   x2: coordenada x del segundo punto
;;   y2: coordenada y del segundo punto
;; Resultado:
;;   Distancia euclidiana entre los dos puntos recibidos
;; Descripción:
;;   Aplica la fórmula para la distancia euclidiana entre puntos
;; Funciones a las que llama: distancia-punto-recta
;;
(define (posicion-circunferencia-recta cx cy r a b c)
  (let ((dist (distancia-punto-recta cx cy a b c))
        )
    (cond ;; Compara el radio con la distancia
      ((> dist r) 1) ;; Posición exterior
      ((< dist r) 2) ;; Posición secante
      (else 3) ;; Posición tangente
      )
    )
  )
;; Ejemplos:
;;(posicion-circunferencia-recta 0 0 3 1 -1 5)
;; = 1 Exterior
;;(posicion-circunferencia-recta 0 0 3 1 -1 3)
;; = 2 Secante
;;(posicion-circunferencia-recta 0 0 3 0 1 -3)
;; = 3 Tangentes
;;;;

;;;;
;; Ejercicio 7
;;;;

;;
;; Nombre: posicion-2-esferas
;; Objetivo:
;;   Determina la posición relativa de dos esferas
;; Parámetros:
;;   cxi: coordenada x del centro i
;;   cyi: coordenada y del centro i
;;   czi: coordenada z del centro i
;;   ri: radio de la esfera i
;; Resultado:
;;   Número que representa la posición relativa entre las esferas
;;   Nota: Iguales(0),Secantes(1),Tangentes interiores(2),Tangentes exteriores(3),
;;         Interiores(4),Exteriores(5),Concéntricas(6)
;; Descripción:
;;   Para determinar la posición relativa entre dos esferas, basta con comparar
;;   sus radios y la distancia entre sus centros.
;;
(define (posicion-2-esferas cx1 cy1 cz1 r1 cx2 cy2 cz2 r2)
  (if (and (= cx1 cx2) (= cy1 cy2) (= cz1 cz2))
      ;; Iguales(0) o concéntricas(6)
      (if (= r1 r2) 0 6)
      ;; Resto de posiciones
      (let ((dist (sqrt (+ (expt (- cx2 cx1) 2) (expt (- cy2 cy1) 2)
                           (expt (- cz2 cz1) 2))))
            (sum-radios (+ r1 r2))
            (dif-radios (abs (- r1 r2)))
            )
        (cond
          ;; Secantes
          ((and (< dist sum-radios) (> dist dif-radios)) 1)
          ;; Tangentes interiores
          ((= dist dif-radios) 2)
          ;; Tangentes exteriores
          ((= dist sum-radios) 3)
          ;; Interiores
          ((< dist dif-radios) 4)
          ;; Exteriores
          (else 5)
          )
        )
      )
  )
;; Ejemplos:
;;(posicion-2-esferas 0 0 0 2 0 0 0 2)
;; = 0 Iguales
;;(posicion-2-esferas 0 0 0 2 0 0 1 2)
;; = 1 Secantes
;;(posicion-2-esferas 0 0 0 2 0 0 1 1)
;; = 2 Tangentes por dentro
;;(posicion-2-esferas 0 0 0 2 0 0 3 1)
;; = 3 Tangentes por fuera
;;(posicion-2-esferas 0 0 0 10 0 0 3 1)
;; = 4 Interiores
;;(posicion-2-esferas 0 0 0 1 0 0 3 1)
;; = 5 Exteriores
;;(posicion-2-esferas 0 0 0 2 0 0 0 4)
;; = 6 Concéntricas
;;;;

;;;;
;; Ejercicio 8
;;;;

;;
;; Nombre: angulo-vectores
;; Objetivo:
;;   Determina el ángulo formado por dos vectores
;; Parámetros:
;;   xi: coordenada x del punto i
;;   yi: coordenada y del punto i
;;   Nota: p1 y p2 forman el primer vector, p3 y p4 el segundo
;; Resultado:
;;   Ángulo que forman los vectores en grados
;; Descripción:
;;   Calcula los vectores y después el ángulo
;;   angulo = arcoseno((ux*vx+uy*vy) / sqrt((ux^2+uy^2)*(vx^2+vy^2)))
;;
(define (angulo-vectores x1 y1 x2 y2 x3 y3 x4 y4)
  (let (;; Cálculo de los vectores u y v formados por los puntos
        (ux (- x2 x1))
        (uy (- y2 y1))
        (vx (- x4 x3))
        (vy (- y4 y3))
        )
    (radians->degrees
       ;; Cálculo del ángulo en radianes
       (acos (/
              (+ (* ux vx) (* uy vy))
              (sqrt (*
                     (+ (expt ux 2) (expt uy 2));;u=2,3
                     (+ (expt vx 2) (expt vy 2));;v=-2,-3
                     )
                    )
              )
             )
       )
    )
  )
;; Ejemplos:
;;(angulo-vectores 0 0 1 0 0 0 1 1)
;; = 45.0
;;(angulo-vectores 0 0 1 0 0 0 -1 0)
;; = 180.0
;;;;

;;;;
;; Ejercicio 9
;;;;

;;
;; Nombre: tipo-cuadrilatero
;; Objetivo:
;;   Determina qué tipo de cuadrilátero forman los cuatro puntos
;; Parámetros:
;;   xi: coordenada x del punto i
;;   yi: coordenada y del punto i
;;   Nota: Los puntos consecutivos deben formar lados
;; Resultado:
;;   Número que representa el tipo de cuadrilátero que forman los puntos
;;   Nota: tipos-> cuadrado(1),rectángulo(2),rombo(3),romboide(4),
;;           trapecio rectangular(5), trapecio isósceles(6),trapecio escaleno(7),
;;           cometa(8),cometa oblicua(9),trapezoide(10)
;; Descripción:
;;   El tipo de cuadrilátero se puede determinar usando el valor de los ángulos
;;   opuestos, la longitud de los lados y si son paralelos entre sí.
;; Funciones a las que llama: angulo-vectores
;;
(define (tipo-cuadrilatero x1 y1 x2 y2 x3 y3 x4 y4)
  ;; Función auxiliar que comprueba si dos lados son paralelos
  (define (paralelos? ax ay bx by cx cy dx dy)
    ;; Si el ángulo es 0 o 180, son paralelos
    (let ((angulo (angulo-vectores ax ay bx by cx cy dx dy))
          )
      (or (= angulo 0) (= angulo 180))
      )
    )
  ;; Los números hacen referencia a los puntos. Ej: 12 del punto 1 al 2
  (let ((angulo-12-14 (angulo-vectores x1 y1 x2 y2 x1 y1 x4 y4))
        (angulo-32-34 (angulo-vectores x3 y3 x2 y2 x3 y3 x4 y4))
        (dist-12 (distancia-euclidiana x1 y1 x2 y2))
        (dist-23 (distancia-euclidiana x2 y2 x3 y3))
        (dist-34 (distancia-euclidiana x3 y3 x4 y4))
        (dist-41 (distancia-euclidiana x4 y4 x1 y1))
        (paralelos-12-34 (paralelos? x1 y1 x2 y2 x3 y3 x4 y4))
        (paralelos-23-41 (paralelos? x2 y2 x3 y3 x4 y4 x1 y1))
        )
    (cond
      ;; Paralelogramos - Lados paralelos dos a dos
      ((and paralelos-12-34 paralelos-23-41)
       ;; Comprueba si todos sus ángulo son rectos
       (if (= angulo-12-14 angulo-32-34 90)
           ;; Comprueba si los lados son iguales
           (if (= dist-12 dist-23 dist-34 dist-41)
               1 ;; Cuadrado - Ángulos rectos y lados iguales
               2 ;; Rectángulo - Ángulos rectos y lados desiguales
               )
           ;; Comprueba si los lados son iguales
           (if (= dist-12 dist-23 dist-34 dist-41)
               3 ;; Rombo - Lados iguales angulo
               4 ;; Romboide - Lados desiguales y ángulo iguales
               )               
           )
       )
      ;; Trapecios - Un par de lados paralelos
      ((or paralelos-12-34 paralelos-23-41)
       (cond
         ;; Trapecio rectangular - Tiene dos ángulos rectos contiguos
         ((or (= angulo-12-14 90) (= angulo-32-34 90)) 5)
         ;; Trapecio isóceles - Dos lados son iguales
         ((or (= dist-12 dist-34) (= dist-23 dist-41)) 6)
         ;; Trapecio escaleno - Sus lados no paralelos no son iguales
         (else 7)
         )
       )
      ;; Cometas y trapezoides - Ningún lado paralelo
      (else
       (let (;; Comprueba si son iguales los lados que salen de cada punto
             (iguales-1 (= dist-12 dist-41))
             (iguales-2 (= dist-23 dist-12))
             (iguales-3 (= dist-34 dist-23))
             (iguales-4 (= dist-41 dist-34))
             )
         (cond
           ;; Cometa - Lados contiguos iguales dos a dos
           ((or (and iguales-1 iguales-3) (and iguales-2 iguales-4)) 8)
           ;; Cometa oblicua - Al un par de lados contiguos iguales
           ((or iguales-1 iguales-2 iguales-3 iguales-4) 9)
           ;; Trapezoide - No tienen lados de igual longitud
           (else 10)
           )
         )
       )
      )
    )
  )
;; Ejemplos:
;;(tipo-cuadrilatero 1 1 1 -1 -1 -1 -1 1)
;; = 1 Cuadrado
;;(tipo-cuadrilatero 2 1 2 -1 -1 -1 -1 1)
;; = 2 Rectángulo
;;(tipo-cuadrilatero 2 0 0 -1 -2 0 0 1)
;; = 3 Rombo
;;(tipo-cuadrilatero -1 0 0 1 2 1 1 0)
;; = 4 Romboide
;;(tipo-cuadrilatero -1 1 0 1 1 0 -1 0)
;; = 5 Trapecio rectangular
;;(tipo-cuadrilatero -0.5 1 0.5 1 1 0 -1 0)
;; = 6 Trapecio isósceles
;;(tipo-cuadrilatero -0.5 1 0.5 1 1 0 -2 0)
;; = 7 Trapecio escaleno
;;(tipo-cuadrilatero 0 1 1 0 0 -2 -1 0)
;; = 8 Cometa
;;(tipo-cuadrilatero 1 1 1 0 0 -2 -1 0)
;; = 9 Cometa oblicua
;;(tipo-cuadrilatero 0 0 -1 2 1 4 2 0.6)
;; = 10 Trapezoide
;;;;
  
;;;;
;; Ejercicio 10
;;;;

;;
;; Nombre: lados_paralelos?
;; Objetivo: determina si dos vectores son paralelos
;; Parámetros:
;;   xi: coordenada x del punto i
;;   yi: coordenada y del punto i
;;   Nota: Los puntos 1 y 2 forman un vector, los 3 y 4 otro vector
;; Resultado:
;;   Se devuelve verdadero si son paralelos y falso en caso contrario
;; Descripción:
;;   El ángulo entre dos vectores paralelos es 0 o 180
;; Funciones a las que llama: angulo-vectores
;;
(define (lados_paralelos? x1 y1 x2 y2 x3 y3 x4 y4)
  ;; Si el ángulo es 0, son paralelos
  (let ((angulo (angulo-vectores x1 y1 x2 y2 x3 y3 x4 y4))
        )
    (or (= angulo 0) (= angulo 180))
    )
  )
;; Ejemplos:
;;(lados_paralelos? 0 0 1 0 0 3 4 3)
;; = #t
;;(lados_paralelos? 0 0 1 0 0 0 0 1)
;; = #f
;;;;

;;
;; Nombre: perpendiculares?
;; Objetivo: determina si dos vectores son perpendiculares
;; Parámetros:
;;   xi: coordenada x del punto i
;;   yi: coordenada y del punto i
;;   Nota: Los puntos 1 y 2 forman un vector, los 3 y 4 otro vector
;; Resultado:
;;   Se devuelve verdadero si son perpendiculares y falso en caso contrario
;; Descripción:
;;   El ángulo entre dos vectores perpendiculares es 90
;; Funciones a las que llama: angulo-vectores
;;
(define (perpendiculares? x1 y1 x2 y2 x3 y3 x4 y4)
  (= 90 (angulo-vectores x1 y1 x2 y2 x3 y3 x4 y4))
  )
;; Ejemplos:
;;(perpendiculares? 0 0 1 0 0 0 1 0)
;; = #f
;;(perpendiculares? 0 0 1 1 0 0 1 -1)
;; = #t
;;;;

;;;;
;; Ejercicio 11
;;;;

;;
;; Nombre: area-rombo
;; Objetivo: halla el área del rombo que forman los puntos recibidos
;; Parámetros:
;;   xi: coordenada x del vértice i
;;   yi: coordenada y del vértice i
;; Resultado:
;;   Valor del área del rombo
;; Descripción:
;;   Primero debe hallar el orden de los lados, viendo cuales forman
;;   las diagonales. Basta con comprobar si son 1-2 y 3-4, si no lo son
;;   serán 1-3 y 2-4. Luego usa: area = diagonal-menor*diagonal-mayor/2
;; Funciones a las que llama: perpendiculares?, distancia-euclidiana
;;
(define (area-rombo x1 y1 x2 y2 x3 y3 x4 y4)
  (let* (;; Comprueba si los pares de puntos 1,2 y 3,4 forman diagonales
         (diagonal-12-34? (perpendiculares? x1 y1 x2 y2 x3 y3 x4 y4))
         ;; Determina la longitud de las diagonales del rombo
         (d1 (if diagonal-12-34?
                 (distancia-euclidiana x1 y1 x2 y2)
                 (distancia-euclidiana x1 y1 x3 y3)
                 )
             )
         (d2 (if diagonal-12-34?
                 (distancia-euclidiana x3 y3 x4 y4)
                 (distancia-euclidiana x2 y2 x4 y4)
                 )
             )
         )
    ;; Área del rombo
    (/ (* d1 d2) 2)
    )
  )
;; Ejemplos:
;;(area-rombo -2 0 0 3 2 0 0 -3)
;; = 12
;;(area-rombo -1 0 1 0 0 2 0 -2)
;; = 4
;;;;

;;;;
;; Ejercicio 12
;;;;

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
;;   Se calculan los coeficientes para la ecuación general de la recta
;;   y se llama a la función distancia-punto-recta.
;;   Cálculo de coeficientes de la recta:
;;     a = y2 - y1    b = x2 - x1    c = x2*y1 - x1*y2
;; Funciones a las que llama: distancia-punto-recta
;;
(define (distancia-punto-recta2 x0 y0 x1 y1 x2 y2)
  (let* (;; Cálculo coeficientes de la recta
         (a (- y2 y1)) ;; a = y2-y1
         (b (- x2 x1)) ;; b = x1-x2
         (c (- (* y1 x2) (* x1 y2))) ;; c = y1*x2 - x1*y2
         )
    (distancia-punto-recta x0 y0 a b c)
    )
  )

;;
;; Nombre: area-trapecio
;; Objetivo: halla el área del trapecio que forman los puntos recibidos
;; Parámetros:
;;   xi: coordenada x del vértice i
;;   yi: coordenada y del vértice i
;; Resultado:
;;   Valor del área del trapecio
;; Descripción:
;;   Primero debe hallar el orden de los lados, viendo cuales forman
;;   las bases. Basta con comprobar si son 1-2 y 3-4 paralelos, si no lo son
;;   serán 1-4 y 2-3.
;; Funciones a las que llama: lados_paralelos?, distancia-euclidiana
;;
(define (area-trapecio x1 y1 x2 y2 x3 y3 x4 y4)
  (let* (;; Comprueba si los pares de puntos 1,2 y 3,4 forman las bases
         (bases-12-34? (lados_paralelos? x1 y1 x2 y2 x3 y3 x4 y4))
         ;; Calcula la longitud de las bases y la altura
         (b1 (if bases-12-34?
                 (distancia-euclidiana x1 y1 x2 y2)
                 (distancia-euclidiana x1 y1 x4 y4)
                 )
             )
         (b2 (if bases-12-34?
                 (distancia-euclidiana x3 y3 x4 y4)
                 (distancia-euclidiana x2 y2 x3 y3)
                 )
             )
         (h (if bases-12-34?
                (distancia-punto-recta2 x3 y3 x1 y1 x2 y2)
                (distancia-punto-recta2 x2 y2 x1 y1 x4 y4)
                )
            )
         )
    ;; Área del rombo
    (/ (* h (+ b1 b2)) 2.0)
    )
  )
;; Ejemplos:
;;(area-trapecio -2 4 2 4 -4 0 4 0)
;; = 24.0
;;(area-trapecio -3 5 3 5 5 0 -5 0)
;; = 40.0
;;;;