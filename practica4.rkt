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
;;
(define (veces lista elem)
  (cond
    ((not (list? lista)) 0)
    ((null? lista) 0)
    ((list? (car lista))
     (+ (veces (car lista) elem)
        (veces (cdr lista) elem)
        )
     )
    (else
     (+ (veces (cdr lista) elem)
        (if (equal? elem (car lista))
            1 ;; Incrementa la cuenta en 1 encuentra el elemento
            0))
     )
    )
  )
;; Ejemplos:
;;(veces '(a (b c) b a (e b (g b h))) 'b)
;; = 4
;;;;

;;;;
;; Ejercicio 7
;;;;

;;
;; Nombre: suprimir
;; Objetivo: eliminar todas la apariciones de un elemento de la lista
;; Parámetros:
;;   lista: lista con posibles sublistas que se modificará
;;   x: elemento que se eliminará de la lista
;; Resultados:
;;   Devuelve la lista después de haber eliminado las aparaciones de x
;; Descripción:
;;   Recorre la lista de forma recursiva y cuando encuentra el valor
;;   a suprimir lo omite de los valores devueltos
;;
(define (suprimir lista x)
  (cond
    ((not (list? lista)) '())
    ((null? lista) '())
    ((list? (car lista))
     (append
      (list (suprimir (car lista) x))
      (suprimir (cdr lista) x)
      )
     )
    (else
     (append
      (if (equal? x (car lista))
          ;; append con lista vacía no altera la otra lista
          '()
          ;; Añade de vuelta el elemento
          (list (car lista))
          )
      ;; LLama la función de nuevo sobre el resto de la lista
      (suprimir (cdr lista) x)
      )
     )
    )
  )
;; Ejemplos:
;;(suprimir '(a b d c (a b a) (d (e g) f) b) 'a)
;; = (b d c (b) (d (e g) f) b)
;;;;

;;;;
;; Ejercicio 8
;;;;

;;
;; Nombre: emparejar
;; Objetivo:
;;   Crea una lista con parejas que compartan el mismo índice en las listas
;;   recibidas como parámetro
;; Parámetros:
;;   lista1: lista cuyos elementos irán a la izquierada de las parejas
;;   lista2: lista cuyos elementos irán a la derecha de las parejas
;; Resultados:
;;   Devuelve una lista con sublistas formadas por los elementos que
;;   tienen el mismo índice en las dos listas recibidas
;; Descripción:
;;   Para funcionar de forma genérica, en caso de listas de distinta
;;   longitud, el número de parejas se limita por la lista más corta.
;;   Luego recorre ambas listas creando parejas y las añade a otra lista.
;;
(define (emparejar lista1 lista2)
  (if (or (null? lista1) (null? lista2))
      '()
      (append (list (car lista1) (car lista2))
              (emparejar (cdr lista1) (cdr lista2))
              )
      )
  )
(define (emparejar lista1 lista2)
  (define (auxiliar lista1 lista2)
    (if (or (null? lista1) (null? lista2))
      '()
      (append (list (car lista1) (car lista2))
              (emparejar (cdr lista1) (cdr lista2))
              )
      )
    )
  (if (and (list? lista1) (list? lista2))
      (auxiliar lista1 lista2)
      '()
      )
  )
;; Ejemplos:
;;(emparejar '(a b c d e) '(1 2 3 4 5))
;; = (a 1 b 2 c 3 d 4 e 5)
;;;;

;;;;
;; Ejercicio 9
;;;;

;;
;; Nombre: cambiar
;; Objetivo: cambiar en la lista recibida un elemento por otro
;; Parámetros:
;;   lista: lista con posibles sublistas en la que cambiar los elementos
;;   obj: elemento a cambiar en la lista
;;   dest: elemento que sustituye al elemento cambiado
;; Resultados:
;;   Devuelve la lista recibida con las ocurrencias de obj cambiadas por dest
;; Descripción:
;;   Recorre de forma recursiva la lista y sublistas, devuelve como elemento
;;   de la lista dest cuando encuentra obj
;;
(define (cambiar lista obj dest)
  (cond
    ((not (list? lista)) '())
    ((null? lista) '())
    ((list? (car lista))
     (append
      (list (cambiar (car lista) obj dest))
      (cambiar (cdr lista) obj dest)
      )
     )
    (else
     (cons
      (if (equal? obj (car lista))
          ;; append con lista vacía no altera la otra lista
          dest
          ;; Añade de vuelta el elemento
          (car lista)
          )
      ;; LLama la función de nuevo sobre el resto de la lista
      (cambiar (cdr lista) obj dest)
      )
     )
    )
  )
;; Ejemplos:
;;(cambiar '(a (a b c) c b (d e b a)) 'a 1)
;; = (1 (1 b c) c b (d e b 1))
;;;;

;;;;
;; Ejercicio 10
;;;;

;;
;; Nombre: cuadrados
;; Objetivo: crea una lista con sublistas de cada elemento recibido y su cuadrado
;; Parámetros:
;;   lista: lista de la que se extraerán los elementos para operar
;; Resultados:
;;   Devuelve una lista compuesta de sublistas con un elemento de la lista
;;   recibida y su correspondiente cuadrado
;; Descripción:
;;   Recorre toda la lista recibida y crea una sublista con el elemento actual
;;   y su cuadrado, va insertando la listas creadas en la sublista a devolver
;;
(define (cuadrados lista)
  (if (null? lista)
      '()
      (cons (list (car lista) (expt (car lista) 2))
            (cuadrados (cdr lista))
            )
      )
  ;;(map (lambda (elem) (list elem (expt elem 2))))
  )
;; Ejemplos:
;;(cuadrados '(1 2 3))
;; = ((1 1) (2 4) (3 9))
;;;;

;;;;
;; Ejercicio 11
;;;;

;;
;; Nombre: dato-resultado
;; Objetivo:
;;   Crea una lista con sublistas emparejando un elemento de la lista recibida
;;   junto con el resultado de pasar ese elemento a la función recibida
;; Parámetros:
;;   lista: lista de la que se obtienen los elementos
;;   func: función que se aplica para crear el segundo elemento de las parejas
;; Resultados:
;;   Devuelve una lista con sublistas cuyo primer elemento es obtenido de la lista
;;   recibida y el segundo es el resultado de pasar ese elemento por la función
;; Descripción:
;;   Se recorre la lista de forma secuencial y para cada elemento se crea una sublista
;;   con el elemento primero y segundo el resultado de aplicarle la función recibida
;;
(define (dato-resultado lista func)
  (if (null? lista)
      '()
      (cons (list (car lista) (func (car lista)))
            (dato-resultado (cdr lista) func)
            )
      )
  ;;(map (lambda (elem) (list elem (func elem))))
  )
;; Ejemplos:
;;(dato-resultado '(1 2 3) (lambda (x) (expt x 2)))
;; = ((1 1) (2 4) (3 9))
;;(dato-resultado '(1 2 3) even?)
;; = ((1 #f) (2 #t) (3 #f))
;;;;

;;;;
;; Ejercicio 12
;;;;

;;
;; Nombre: diferencia
;; Objetivo:
;;   Obtener una lista con los elementos de la lista recibida que no están en
;;   otra lista recibida
;; Parámetros:
;;   lista-obj: lista de la que extraer elementos
;;   lista-dif: lista cuyos elementos se eliminan de la primera
;; Resultados:
;;   Devuelve una lista que sea la diferencia de la primera con la segunda
;; Descripción:
;;
;;
(define (diferencia lista-obj lista-dif)
  
  )


;;;;;; PREGUNTAR:
;; funciones compactas recursivas: cuadrados, dato-resultado
;; posible usar map en ejercicios 10 y 11

