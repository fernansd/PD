;;
;; Fichero: "practica4.rkt"
;; Descripción: Ejercicios de la práctica 4
;; Autor: Fernando Sánchez Delgado
;;

(require racket/vector)

;;;;
;; Ejercicio 1
;;;;

;;
;; Nombre: producto-escalar
;; Objetivo: calcula el producto escalar de dos vectores
;; Parámetros:
;;   v1: primer vector
;;   v2: segundo vector
;;   Nota: si no son de igual longitud se ignoran las componentes extras del más largo
;; Resultado:
;;   Devuelve el producto escalar de los vectores
;; Descripción:
;;   El producto escalar se calcula sumando el producto de las componentes de un
;;   vector con las correspondientes en otro vector
;;
(define (producto-escalar v1 v2)
  (do (;; Variable de posición
       (i 0 (+ i 1))
       (tam (- (min (vector-length v1) (vector-length v2)) 1))
       (resultado 0 (+ resultado (* (vector-ref v1 i) (vector-ref v2 i))))
       )
    ((> i tam) resultado)
    )
  )
;; Ejemplos:
;;(producto-escalar #(1 0 2) #(1 2 3))
;; = 7
;;(producto-escalar #(1 0 2 0 1) #(1 2 3 4 5))
;; = 12
;;;;

;;;;
;; Ejercicio 2
;;;;

;;
;; Nombre: producto-vectorial
;; Objetivo: calculo el producto vectorial entre dos vectores
;; Parámetros:
;;   u: primer vector
;;   v: segundo vector
;; Resultados:
;;   Devuelve un vector que es el producto vectorial de los vectores
;; Descripción:
;;   Para el producto vectorial en el espacio R^3 dados dos vectores u y v se usa:
;;    r_x = u_y*v_z - u_z*v_y
;;    r_y = u_z*v_x - u_x*v_z
;;    r_z = u_x*v_y - u_y*v_x
;;
(define (producto-vectorial u v)
  (vector ;; Vector resultado
    ;; r_x = u_y*v_z - u_z*v_y
    (- (* (vector-ref u 1) (vector-ref v 2)) (* (vector-ref u 2) (vector-ref v 1)))
    ;; r_y = u_z*v_x - u_x*v_z
    (- (* (vector-ref u 2) (vector-ref v 0)) (* (vector-ref u 0)(vector-ref v 2)))
    ;; r_z = u_x*v_y - u_y*v_x
    (- (* (vector-ref u 0) (vector-ref v 1)) (* (vector-ref u 1) (vector-ref v 0)))
    )
  )
;; Ejemplos:
;;(producto-vectorial #(2 0 1) #(1 -1 3))
;; = #(1 -5 -2)
;;;;

;;;;
;; Ejercicio 3
;;;;

;;
;; Nombre: aplicar
;; Objetivo: realiza el producto vectorial del vector por las columnas de la matriz
;; Parámetros:
;;   vec: vector a multiplicar
;;   mat: matriz a transformar
;; Resultados:
;;   Matriz transformada al realizar el producto vectorial de cada vector columna
;;   por el vector recibido
;; Descripción:
;;   
;;
(define (aplicar vec mat)
  (let* (
         (fils-1 (vector-length mat))
         (cols-1 (vector-length (vector-ref mat 0)))
         )
    (if (= fils-1 (- (vector-length vec) 1))
        (do (
             (ncol 0 (if (= fil fils-1) (+ col 1) col))
             (col (vector-map (lambda (col) 3))
                  (vector-map (lambda (col) (vector-ref col ncol)) mat))
             (resultado '() (cons (producto-escalar vec col) resultado))
             )
          ((> ncol cols-1) resultado)
          )
        ;; Devuelve #f si no son multiplicables
        #f
        )
    )
  )

;;;;
;; Ejercicio 4
;;;;

;;
;; Nombre: media-vector
;; Objetivo: calcula la media aritmética de los valores del vector
;; Parámetros:
;;   vec: vector a recorrer
;; Resultados:
;;   Devuelve el valor de la media aritmética o #f si está vacío
;; Descripción:
;;   Suma todos los elementos del vector y divide la suma por la longitud
;;   del vector.
;;
(define (media-vector vec)
  (define len (vector-length vec))
  (if (= 0 len)
      #f
      (do (
           (i 0 (+ i 1))
           (suma 0.0 (+ suma (vector-ref vec i)))
           )
        ((= i len) (/ suma len))
        )
      )
  )
;; Ejemplos:
;;(media-vector #(2 4 8 5 7))
;; = 5.2
;;;;

;;
;; Nombre: minimax
;; Objetivo: devuelve el menor elemento de entre los mayores de las filas de la matriz
;; Parámetros:
;;   mat: matriz sobre la que se opera
;; Resultados:
;;   Se devuelve el menor elemento de entre los mayores de cada fila de la matriz
;; Descripción:
;;   Primero halla el mayor de cada fila y luego busca el menor de entre esos mayores
;;
(define (minimax mat)
  (do (
       (fils (vector-length mat))
       (i 0 (+ i 1))
       (max-valores '() (cons (apply max (vector->list (vector-ref mat i))) max-valores))
       )
    ((= i fils) (apply min max-valores))
    )
  )
;; Ejemplos
;; (minimax #(#(1 2 3) #(4 5 6) #(7 8 9)))
;; = 3
;;;;

;;;;
;; Ejercicio 5
;;;;

;;
;; Nombre: primo?
;; Objetivo: determina si un número es primo o no
;; Parámetros:
;;   num: número para evaluar
;; Resultados:
;;   Devuelve verdadero si el número es primo, falso en caso contrario
;; Descripción:
;;   Un número será primo si no tiene divisores propios menores que su raíz cuadrada
;;
(define (primo? num)
  (do (
       (max-div (floor (sqrt num))) ;; máximo divisor posible
       (i 1 (+ i 1)) ;; contador de iteraciones
       (div 2 (+ (* 2 i) 1)) ;; divisor actual
       (divisor? #f (= 0 (remainder num div)))
       )
    ;; sale si se ha encontrado divisor o se ha superado el máximo divisor posible
    ((or divisor? (> div max-div)) (not divisor?))
    )
  )

;;
;; Nombre: filtrar-primos
;; Objetivo: encontrar los números primos en una lista
;; Parámetros:
;;   lista: lista en la que buscar los primos
;; Resultados:
;;   Devuelve una lista con todos los números primos encontrados
;; Descripción:
;;   Recorre la lista entera y comprueba la primalidad de todos los elementos
;; Funciones a las que llama: primo?
;;
(define (filtrar-primos lista)
  (define (auxiliar primos lista)
    (if (null? lista)
        primos
        (auxiliar
         (if (primo? (car lista))
             (append primos (list (car lista)))
             primos
             )
         (cdr lista))
        )
    )
  (auxiliar '() lista)
  )
;; Ejemplos:
;;(filtrar-primos '(2 4 5 15 17 33))
;; = (2 5 17)
;;;;

;;;;
;; Ejercicio 6
;;;;

;;
;; Nombre: veces
;; Objetivo: encuenta las ocurrencias de un elemento en una lista
;; Parámetros:
;;   lista: lista donde buscar el elemento
;;   elem: elemento a buscar dentro de la lista
;; Resultados:
;;   Devuelve el número de veces que se ha encontrado el elemento en la lista
;; Descripción:
;;   Recorre toda la lista y va incrementando un conta