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
       ;; El vector más pequeño limita el número de iteraciones
       (tam (- (min (vector-length v1) (vector-length v2)) 1))
       ;; Añade al resultado el producto de las componentes de los vectores
       (resultado 0 (+ resultado (* (vector-ref v1 i) (vector-ref v2 i))))
       )
    ;; Cuando se haya recorrido el vector más pequeño, se para
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
;;   Se numerarán las componentes, siendo x índice 0 y así sucesivamente
;;
(define (producto-vectorial u v)
  ;; Vector de resultados
  (define vec (make-vector 3))
  (do (
       (len (vector-length vec))
       ;; Se comienza con el índice en la componente y
       (i 1 (if (= i 2) 0 (+ i 1)))
       ;; Se comienza con el índice en la componente z
       (j 2 (if (= j 2) 0 (+ j 1)))
       ;; Contador de iteraciones
       (iter 0 (+ iter 1))
       )
    ;; Para tras calcular todas las componentes
    ((= iter len) vec)
    ;; Calcula la posición iter con: u_i*v_j - u_i*v_j
    (vector-set! vec iter (-
                           (* (vector-ref u i) (vector-ref v j))
                           (* (vector-ref u j) (vector-ref v i))
                           )
                 )
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
;;   Ya que la matriz recibida tiene la forma de un vector de filas, lo primero en cada
;;   iteración es obtener el vector columna. Luego se le aplica el vector recibido como
;;   parámetro realizando el producto escalar
;;
(define (aplicar vec mat)
  ;; Obtiene el número de columnas
  (define ncol (vector-length (vector-ref mat 0)))
  (define (auxiliar resultado pos)
    (cond
      ;; Comprueba si aún le quedan iteraciones
      ((< pos ncol)
       ;; Calcula el elemento de la columna de índice pos
       (vector-set! resultado pos
                    (producto-escalar vec
                                      ;; Obtiene el vector columna de índice pos
                                      (vector-map (lambda (fila) (vector-ref fila pos)) mat)))
       (auxiliar resultado (+ pos 1))
       )
      ;; Devuelve el resultado si ya ha terminado de calcular
      (else resultado)
      )
    )
  (auxiliar (make-vector ncol) 0)
  )
;; Ejemplos:
;;(aplicar #(1 1 1) #(#(1 2 4) #(3 4 8) #(5 6 12)))
;; = #(9 12 24)
;;;;

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
  ;; Comprueba si el vector tiene al menos longitud 1
  (if (= 0 len)
      #f
      (do (
           ;; Contador de iteraciones
           (i 0 (+ i 1))
           ;; Sumatorio de los elementos del vector
           (suma 0.0 (+ suma (vector-ref vec i)))
           )
        ;; La media es el sumatorio entre el número de elementos
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
       ;; Variable contador de iteraciones
       (i 0 (+ i 1))
       ;; Se crea una lista que contenga los mayores elementos de cada fila
       (max-valores '() (cons (apply max (vector->list (vector-ref mat i))) max-valores))
       )
    ;; Se calcula el mínimo elemento entre los mayores
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
  ;; Función auxiliar que recibe una lista donde guardar los primos
  ;; y otra donde buscarlos
  (define (auxiliar primos lista)
    ;; La condición de parada es que no hay elementos en la lista de búsqueda
    (if (null? lista)
        primos ;; Al acabar devuelve la lista de primos
        ;; Si no ha terminado vuelve a llamar la función
        (auxiliar
         ;; Comprueba si debe añadirlo o no a la lista de primos
         (if (primo? (car lista))
             (append primos (list (car lista)))
             primos
             )
         ;; Quita la cabeza de la lista
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
    ;; Comprueba si no se ha recibido una lista
    ((not (list? lista)) 0)
    ;; Comprueba si la lista no tiene elementos
    ((null? lista) 0)
    ;; Comprueba si hay una sublista
    ((list? (car lista))
     ;; Calcula la suma del resultado de llamar a la sublista y al resto de la lista
     (+ (veces (car lista) elem)
        (veces (cdr lista) elem)
        )
     )
    ;; Si la cabeza de lista es un elemento normal
    (else
     ;; LLama recursivamente al resto de la lista
     (+ (veces (cdr lista) elem)
        ;; Comprueba si hay encontrado una ocurrencia o no
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
    ;; Comprueba si no se ha recibido una lista
    ((not (list? lista)) '())
    ;; Comprueba si no queda elementos en la lista
    ((null? lista) '())
    ;; Comprueba si ha sublista
    ((list? (car lista))
     (append
      (list (suprimir (car lista) x))
      (suprimir (cdr lista) x)
      )
     )
    ;; Si es un elemento normal
    (else
     ;; Devuelve el resultado de llamarla sobre el resto de la lista
     (append
      ;; Comprueba si debe omitir el elemento de la lista
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
  ;; Para si alguna de las dos listas ya no tiene elementos
  (if (or (null? lista1) (null? lista2))
      '() ;; No devuelve nada si ya no puede seguir
      ;; Empareja las cabezas de lista
      (append (list (car lista1) (car lista2))
              ;; Vuelve a llamar sobre el resto de las listas
              (emparejar (cdr lista1) (cdr lista2))
              )
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
    ;; Comprueba si no se ha recibido una lista
    ((not (list? lista)) '())
    ;; Comprueba si la lista está vacía
    ((null? lista) '())
    ;; Comprueba si hay una sublista
    ((list? (car lista))
     (append
      (list (cambiar (car lista) obj dest))
      (cambiar (cdr lista) obj dest)
      )
     )
    ;; Si es un elemento general
    (else
     ;; Añade al resultado el valor cambiado
     (cons
      ;; Comprueba si tiene que introducir el valor cambiado o dejarlo igual
      (if (equal? obj (car lista))
          ;; Sustituye por el valor
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
  ;; Se detiene cuando no quedan elementos en la lista
  (if (null? lista)
      '() ;; No devuelve nada si no puede seguir
      ;; Calcula el cuadrado de la cabeza de lista
      (cons (list (car lista) (expt (car lista) 2))
            ;; Se llama de nuevo sobre el resto de la lista
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
  ;; Comprueba si no quedan elementos
  (if (null? lista)
      '() ;; No devuelve nada si no queda elementos en la lista
      ;; Empareja el elemento con el resultado de aplicarle la función
      (cons (list (car lista) (func (car lista)))
            ;; Se llama sobre el resto de la lista
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
;;   Recorre la lista de dónde se extraen los elementos y comprueba si cada
;;   elemento se encuentra en la otra lista
;;
(define (diferencia lista-obj lista-dif)
  ;; Comprueba si no quedan elementos en la lista
  (if (null? lista-obj)
      '() ;; No devuelve nada si no quedan elementos
      (append
       ;; Vuelve a llamar sobre el resto de la lista
       (diferencia (cdr lista-obj) lista-dif)
       ;; Comprueba si el elemento es miembro de la lista
       (if (member (car lista-obj) lista-dif)
           '()
           (list (car lista-obj))
           )
       )
      )
  )
;; Ejemplos:
;;(diferencia '(libro Sol casa Luna) '(Sol Marte Luna))
;; = (libro casa)
;;;;

;;;;
;; Ejercicio 13
;;;;

;;
;; Nombre: diferencia-simetrica
;; Objetivo:
;;   Genera una lista con los elementos no comunes entre la listas recibidas
;;   como parámetro
;; Parámetros:
;;   lista1: una de las listas para realizar la diferencia
;;   lista2: la otra lista con la que se realiza la diferencia
;; Resultados:
;;   Devuelve una lista compuesta por los elementos no comunes entre las dos listas
;; Descripción:
;;   Recorre ambas listas y incluyen en el resultado: los elementos de la lista1 que
;;   no estén en la lista2 y los elementos de la lista2 que no estén en la lista1
;; Funciones a las que llama: diferencia
;;
(define (diferencia-simetrica lista1 lista2)
  ;; Llama a la función diferencia de cada lista con la otra
  (append
   (diferencia lista1 lista2)
   (diferencia lista2 lista1)
   )
  )
;; Ejemplos:
;;(diferencia-simetrica '(libro Sol casa Luna) '(Sol Marte Luna))
;; = (casa libro Marte)
;;;;

;;;;
;; Ejercicio 14
;;;;

;;
;; Nombre: media-aritmetica-lista
;; Objetivo: realizar la media aritmética de los elementos de una lista
;; Parámetros:
;;   lista: lista a cuyos elementos se les hace la media aritmética
;; Resultados:
;;   Devuelve la media aritmética de los elementos de la lista recibida
;; Descripción:
;;   Recorre la lista sumando todos los elementos y divide el resultado entre
;;   la longitud de la lista
;;
(define (media-aritmetica-lista lista)
  ;; media = sum(lista)/len(lista)
  (/
   ;; Calcula el sumatorio de toda la lista
   (apply + lista)
   1.0 ;; para convertir a decimal el resultado
   (length lista)
   )
  )
;; Ejemplos
;;(media-aritmetica-lista '(1 2 3 4 5))
;; = 3.0
;;;;

;;
;; Nombre: media-aritmetica
;; Objetivo: calcular la media aritmética de los números recibidos
;; Parámetros:
;;   ni: número recibido en la posición i
;;   Nota: recibe un número arbitrario de parámetros
;; Resultados:
;;   Devuelve la media aritmética de los parámetros recibidos
;; Descripción:
;;   Al recibir los parámetros como una lista simplemente usa la función
;;   que calcula la media aritmética de una lista
;; Funciones a las que llama: media-aritmetica-lista
;;
(define media-aritmetica
  (lambda lista
    ;; Llama la función de media de una lista sobre la lista de argumentos
    (media-aritmetica-lista lista)
    )
  )
;; Ejemplos:
;;(media-aritmetica 1 2 3 4 5)
;; = 3.0
;;;;

;;
;; Nombre: media-aritmetica-bis
;; Objetivo: calcular la media aritmética de los números recibidos
;; Parámetros:
;;   ni: número recibido en la posición i
;;   Nota: se pide un mínimo de dos números como parámetro
;; Resultados:
;;   Devuelve la media aritmética de los parámetros recibidos
;; Descripción:
;;   Al recibir los parámetros como una lista simplemente usa la función
;;   que calcula la media aritmética de una lista
;; Funciones a las que llama: media-aritmetica-lista
;;
(define media-aritmetica-bis
  (lambda (n1 n2 . lista)
    ;; Añade los parámetros obligatorios a la lista y llama la función
    ;; que calcula media sobre una lista
    (media-aritmetica-lista (append (list n1 n2) lista))
    )
  )
;; Ejemplos:
;;(media-aritmetica-bis 1 2 3 4 5)
;; = 3.0
;;;;

;;;;
;; Ejercicio 15
;;;;

;;
;; Nombre: area-poligono
;; Objetivo: calcula el área del polígono cuyos vértices se reciben
;; Parámetros:
;;   xi: coordenada x del vértice i
;;   yi: coordenada y del vértice i
;;   Nota: se requiere un mínimo de tres vértices (coordenadas x e y)
;; Resultados:
;;   Devuelve el área delimitada por los vértices recibidos
;; Descripción:
;;   Usa la fórmula del área de Gauss para un polígono con cualquier número
;;   de lados. La fórmula usada es:
;;     Área = 1/2*sumatorio(determinante((xi,yi),(xi+1,yi+1)))
;;
(define area-poligono
  (lambda (x1 y1 x2 y2 x3 y3 . resto)
    ;; Se añade al final el primer punto para simplificar los cálculos
    (define vertices
      (append (list x1 y1 x2 y2 x3 y3) resto (list x1 y1))
      )
    ;; Se resta 1 ya que se repite el primer punto
    (define nvertices (- (/ (length vertices) 2) 1))
    ;; Función auxiliar que calcula el sumatorio de los determinantes
    (define (auxiliar vertices it)
      (if (< it nvertices)
          ;; Sumatorio de determinantes
          (+
           ;; Cálculo del determinante
           (- (* (car vertices) (cadddr vertices)) (* (caddr vertices) (cadr vertices)))
           ;; Realiza la siguiente iteración
           (auxiliar (cddr vertices) (+ it 1))
           )
          0.0 ;; Cuando llega al final devuelve 0
          )
      )
    ;; Calcula el área
    (/ (abs (auxiliar vertices 0)) 2)
    )
  )
;; Ejemplos:
;;(area-poligono 1 0 0 2 -1 0)
;; = 2.0
;;(area-poligono 4 3 1 0 0 -1 3 0)
;; = 4.0
;;;;
