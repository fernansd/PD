;;
;; Fichero: "practica3.rkt"
;; Descripción: Ejercicios de la práctica 3
;; Autor: Fernando Sánchez Delgado

;;;;
;; Ejercicio 1
;;;;

;;
;; Nombre: suma-divisores
;; Objetivo: sumar todos los divisores del número proporcionado
;; Parámetros:
;;   num: número al que calcular los divisores
;; Resultados:
;;   Suma de los divisores
;; Descripción:
;;   Los divisores son todos aquellos números que dividen al número original
;;   con resto 0. Un divisor nunca será mayor que la mitad del número.
;;
(define (suma-divisores num)
  ;; Comprueba si div es divisor de n
  (define (divisor? n div)
    (= 0 (remainder n div)))
  (do
      (
       (div 2 (+ 1 div))
       (suma 1 (if (divisor? num div)
                   (+ suma div)
                   suma)
            )
       (ultimo-div (ceiling (/ num 2)))
       )
    ((> div ultimo-div) suma)
    )
  
  )
;; Ejemplos:
;;(suma-divisores 220)
;; = 284
;;(suma-divisores 284)
;; = 220
;;;;

;;
;; Nombre: amigos?
;; Objetivo: comprueba si los números proporcionados son amigos
;; Parámetros:
;;   num1: primer número entero
;;   num2: segundo número entero
;; Resultados:
;;   Devuelve #t en caso de que sean amigos y #f en caso contrario
;; Descripción:
;;   Dos números son amigos si la suma de divisores de uno es igual al otro
;;   número y viceversa.
;; Funciones a las que llama: suma-divisores
;;
(define (amigos? num1 num2)
  (and 
   (= num1 (suma-divisores num2))
   (= num2 (suma-divisores num1))
   )
  )
;; Ejemplos:
;;(amigos? 220 284)
;; = #t
;;(amigos? 90 13)
;; = #f
;;;;

;;
;; Nombre: perfecto?
;; Objetivo: comprueba si el número proporcionado es perfecto
;; Parámetros:
;;   num: número entero
;; Resultados:
;;   Devuelve #t en caso de que sea perfecto y #f en caso contrario
;; Descripción:
;;   Un número es perfecto si es amigo con la suma de sus divisores
;; Funciones a las que llama: suma-divisores, amigos?
;;
(define (perfecto? num)
  (amigos? num (suma-divisores num))
  )
;; Ejemplos:
;;(perfecto? 28)
;; = #t
;;(perfecto? 17)
;; = #f
;;;;;

;;;;
;; Ejercicio 2
;;;;

;;
;; Nombre: mcd-iterativo
;; Objetivo: calcular el máximo común divisor de los números recibidos
;; Parámetros:
;;   num1: primer número entero
;;   num2: segundo número entero
;; Resultados:
;;   Devuelve el máximo común divisor de los números
;; Descripción:
;;   Dados dos números a y b se obtiene el resto de su división.
;;   Pasando a nombrarse "a" el número "b" y a nombrarse "b" el resto.
;;   Este proceso se realiza repetidamente hasta que b sea 0, momento en el
;;   cual se sabe que el número que sea "a", será el máximo común divisor
;;   de los a y b originales. Usa un procedimiento iterativo.
;;
(define (mcd-iterativo num1 num2)
  (do
      (;; El mayor número se debe asignar a "a"
       (a (if (> num1 num2) num1 num2) b)
       (b (if (<= num1 num2) num1 num2) (remainder a b))
       )
    ((= b 0) a)
    )
  )
;; Ejemplos:
;;(mcd-iterativo 630 198)
;; = 18
;;(mcd-iterativo 40 21)
;; = 1
;;;;

;;
;; Nombre: mcd-recursivo
;; Objetivo: calcular el máximo común divisor de los números recibidos
;; Parámetros:
;;   num1: primer número entero
;;   num2: segundo número entero
;; Resultados:
;;   Devuelve el máximo común divisor de los números
;; Descripción:
;;   Dados dos números a y b se obtiene el resto de su división.
;;   Pasando a nombrarse "a" el número "b" y a nombrarse "b" el resto.
;;   Este proceso se realiza repetidamente hasta que b sea 0, momento en el
;;   cual se sabe que el número que sea "a", será el máximo común divisor
;;   de los a y b originales. Usa un procedimiento recursivo.
;;
(define (mcd-recursivo num1 num2)
  ;; Función auxiliar que se encarga de calcular el MCD
  (define (mcd-aux n1 n2)
    ;; Se termina cuando n2 = 0
    (if (= 0 n2)
        n1
        (mcd-aux n2 (remainder n1 n2))
        )
    )
  (let
      (;; Asigna a la variable "a" el mayor número, el menor a "b"
       (a (if (> num1 num2) num1 num2))
       (b (if (<= num1 num2) num1 num2))
       )
    (mcd-aux a b)
    )
  )
;; Ejemplos:
;;(mcd-recursivo 630 198)
;; = 18
;;(mcd-recursivo 40 21)
;; = 1
;;;;

;;;;
;; Ejercicio 3
;;;;

;;
;; Nombre: primo-iterativo?
;; Objetivo: determina si un número es primo o no
;; Parámetros:
;;   num: número para evaluar
;; Resultados:
;;   Devuelve verdadero si el número es primo, falso en caso contrario
;; Descripción:
;;   Un número será primo si no tiene divisores propios menores que su raíz cuadrada
;;
(define (primo-iterativo? num)
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
;; Ejemplos
;;(map primo-iterativo? '(3 7 17 31 43 149 151 653))
;; = (#t #t #t #t #t #t #t #t)
;;(map primo-iterativo? '(4 26 63 84 235))
;; = (#f #f #f #f #f)
;;;;

;;
;; Nombre: primo-recursivo?
;; Objetivo: determina si un número es primo o no
;; Parámetros:
;;   num: número para evaluar
;; Resultados:
;;   Devuelve verdadero si el número es primo, falso en caso contrario
;; Descripción:
;;   Un número será primo si no tiene divisores propios menores que su raíz cuadrada
;;
(define (primo-recursivo? num)
  ;; función auxiliar que recibe el número, el divisor y el máximo divisor
  (define (primo-aux num div max-div)
    ;; comprueba si ya hemos superado el máximo divisor
    (if (> div max-div)
        #t ;; si  llega aquí no ha tenido divisores, por lo que es primo
        ;; comprueba si hay divisor, sino lo hay pasa al siguiente
        (if (= 0 (remainder num div))
            #f ;; Si existe divisor no es primo
            (primo-aux num (+ div 2) max-div)
            )
        )
    )
  ;; Ya que 2 es el único número par, se hace por separado
  (if (= 0 (remainder num 2))
      #f
      (primo-aux num 3 (floor (sqrt num)))
      )
  )
;; Ejemplos
;;(map primo-recursivo? '(3 7 17 31 43 149 151 653))
;; = (#t #t #t #t #t #t #t #t)
;;(map primo-recursivo? '(4 26 63 84 235))
;; = (#f #f #f #f #f)
;;;;

;;;;
;; Ejercicio 4
;;;;

;;
;; Nombre: integral
;; Objetivo:
;;   Calcula la integral definida en el intervalo, aproximándola por el
;;   método de los trapecios
;; Parámetros:
;;   a: extremo izquierdo del intervalo
;;   b: extremo derecho del intervalo
;;   f: función a la que calcular la integral
;;   n: número de trapecios con los que aproximar
;; Resultados:
;;   Valor aproximado de la integral en el intervalo
;; Descripción:
;;   Usa el método de los trapecios para realizar la aproximación. El método
;;   divide el intervalo en un número de trapecio y calcula la suma de sus áreas.
;;   integral(f, [a,b]) = sum([0,n-1], (f(x_i) + f(x_{i+1}))*h/2)
;;     h = (b-a)/n   x_i = a + i*h
;;
(define (integral a b f n)
  ;; ancho de los trapecios
  (define h (/ (- b a) n))
  (do (
       ;; variable contador
       (i 1 (+ i 1))
       ;; termino actual
       (xi (+ a (* 0 h)) (+ a (* i h)))
       ;; termino siguiente
       (xi+1 (+ a (* 1 h)) (+ a (* (+ i 1) h)))
       ;; suma de las áreas
       (sum 0 (+ sum (* h (/ (+ (f xi) (f xi+1)) 2.))))
       )
    ((> i n) sum)
    )
  )

;; Apartado b:
;;(integral 0 2 (lambda (x) (+ 1 (* 3 (expt x 2)))) 300)
;; = 10.000044444444443

;; Apartado c:
;; Función auxiliar que suma un serie
(define (sumar-serie-trapecios iteraciones)
  (do (
       (n 0 (+ n 1))
       (resultado 0.0 (+ resultado (termino x n)))
       )
    ((> n iteraciones) resultado)
    )
  )
;;(sumar-serie-trapecios 300)

;;;;
;; Ejercicio 5
;;;;

(define (suma-serie-iterativa f inicial siguiente cota)
  (do (
       (t 3)
       )
    ((> t 3) 8)
    )
  )


;;;;
;; Ejercicio 6
;;;;

;;
;; Nombre: termino-numero-e
;; Objetivo: calcular el valor del término n en la sucesión
;; Parámetros:
;;   n: posición del término a calcular en la sucesión
;; Resultados:
;;   Valor del término n de la sucesión
;; Descripción:
;;   Usa la fórmula que determina un término genérico de la sucesión:
;;     termino(n) = (1 + 1/n)^n
;;
(define (termino-numero-e n)
  (expt (+ 1 (/ 1.0 n)) n)
  )
;; Ejemplos:
;;(termino-numero-e 50)
;; = 2.691588029073608
;;(termino-numero-e 10000)
;; = 2.7181459268249255
;;;;

;;
;; Nombre: limite-sucesion-numero-e-iterativa
;; Objetivo: calcula el limite de la sucesión que converge al número e
;; Parámetros:
;;   termino: función que representa un término general de la sucesión
;;   cota: valor de cota mínima de error para el límite
;; Resultados:
;;   Valor aproximado del número e, usando el límite de la sucesión
;;   que calcula su valor.
;; Descripción:
;;   Se siguen calculando términos hasta que la distancia entre dos términos
;;   de la sucesión, sea menor que la cota proporcionada: abs(a_{n+1}-a_n) < cota
;; Funciones a las que llama: termino-numero-e
;;
(define (limite-sucesion-numero-e-iterativa cota)
  (do (
       ;; posición del término a calcular
       (i 2 (+ i 1))
       ;; valor del término actual
       (a_n (termino-numero-e 1) (termino-numero-e i))
       ;; valor del término siguiente
       (a_n+1 (termino-numero-e 2) (termino-numero-e (+ i 1)))
       ;; diferencia entre los términos consecutivos
       (error cota (- a_n+1 a_n))
       )
    ;; Se sale cuando la diferencia entre dos términos sea menor que la cota recibida
    ((< error cota) a_n)
    )
  )
;; Ejemplos:
;;(limite-sucesion-numero-e-iterativa 0.001)
;; = 2.6824354773085255
;;(limite-sucesion-numero-e-iterativa 0.0000001)
;; = 2.7179132895130134
;;;;

;;;;
;; Ejercicio 7
;;;;

;;
;; Nombre: limite-iterativa
;; Objetivo: calcular el límite para una sucesión númerica general
;; Parámetros:
;;   termino: función que representa un término general de la sucesión
;;   cota: valor de cota mínima de error para el límite
;; Resultados:
;;   Valor del límite para la sucesión que cumple la cota proporcionada
;; Descripción:
;;   Sigue calculando términos de la serie hasta que la diferencia entre el término
;;   actual y el siguiente, es menor que la cota de error recibida
;;
(define (limite-iterativa termino cota)
  (do (
       ;; posición del término a calcular
       (i 2 (+ i 1))
       ;; valor del término actual
       (a_n (termino 1) (termino i))
       ;; valor del término siguiente
       (a_n+1 (termino 2) (termino (+ i 1)))
       ;; diferencia entre los términos consecutivos
       (error cota (- a_n+1 a_n))
       )
    ;; Se sale cuando la diferencia entre dos términos sea menor que la cota recibida
    ((< error cota) a_n)
    )
  )
;; Apartado b (ejemplo):
;;(limite-iterativa termino-numero-e 0.00001)
;; = 2.7146076461849074
;;(limite-sucesion-numero-e-iterativa 0.00001)
;; = 2.7146076461849074
;;;;

;;;;
;; Ejercicio 8
;;;;

;;
;; Nombre: suma-aureo-iterativo
;; Objetivo: calcular el número aúreo usando la cantidad de términos que se pide
;; Parámetros:
;;   n: número se sumandos a utilizar en la suma
;; Resultados:
;;   Valor del número aúreo aproximado al sumar la cantidad de términos
;;   de la suma que se pide.
;; Descripción:
;;   Se aproxima el valor del número aúreo al sumar la cantidad de términos
;;   de la suma que se pide: n_áureo = sqrt(1+sqrt(1+sqrt(1+sqrt(1+....))))
;;   Se realiza de forma iterativa.
;;
(define (suma-aureo-iterativo n)
  (do (
       ;; Número de la iteración
       (i 1 (+ i 1))
       ;; Valor de la suma para la iteración
       (suma 1 (sqrt (+ 1 suma)))
       )
    ((>= i n) suma) ;; Devuelve la suma al acabar
    )
  )

;;
;; Nombre: suma-aureo-recursivo
;; Objetivo: calcular el número aúreo usando la cantidad de términos que se pide
;; Parámetros:
;;   n: número se sumandos a utilizar en la suma
;; Resultados:
;;   Valor del número aúreo aproximado al sumar la cantidad de términos
;;   de la suma que se pide.
;; Descripción:
;;   Se aproxima el valor del número aúreo al sumar la cantidad de términos
;;   de la suma que se pide: n_aureo = sqrt(1+sqrt(1+sqrt(1+sqrt(1+....))))
;;   Se realiza de forma recursiva.
;;
(define (suma-aureo-recursivo n)
  (if (= n 1)
      1 ;; El último término vale 1
      (sqrt (+ 1 (suma-aureo-recursivo (- n 1))))
      )
  )

;;;;
;; Ejercicio 9
;;;;

;;
;; Nombre: calculo-pi-iterativo
;; Objetivo: calcular el número pi usando la cantidad de términos que se pide
;; Parámetros:
;;   n: número de fracciones continuas que se deben calcular
;; Resultados:
;;   Valor de pi aproximado mediante la fracción continua
;; Descripción:
;;   Se suman tantos términos de la fracción continua como se piden por parámetro.
;;   La fracción tiene la forma:
;;     pi = 4/(1+ 1/(3+ 4/(5+ 9/(7+..))))
;;
(define (calculo-pi-iterativo n)
  (do (
       (i (- n 1) (- i 1))
       (numerador (expt n 2.0) (expt i 2.0))
       (denominador (+ 1 (* 2 (- n 1))) (- denominador 2))
       (resultado (+ 1 (* 2. (- n 1))) (+ denominador (/ numerador resultado)))
       )
    ((< i 0) (/ 4 resultado))
    )
  )
;; Ejemplos:
;;(calculo-pi-iterativo 10)
;; = 3.1415926843825424
;;(calculo-pi-iterativo 50)
;; = 3.141592653589793
;;;;

;;;;
;; Ejercicio 10
;;;;

;;
;; Nombre: factor-wallis
;; Objetivo: calcula el factor para una posición de la sucesión de Wallis
;; Parámetros:
;;   n: posición el factor a calcular
;;   Nota: para n < 1 no existen valores en el producto de Wallis
;; Resultados:
;;   Valor del factor en la posición n de la sucesión de Wallis
;; Descripción:
;;   La sucesión de Wallis tiene la forma:
;;     (2/3)*(4/3)*(4/5)*(6/5)*(6/7)*(8/7)*(8/9)*...
;;
(define (factor-wallis n)
  (if (= 0 (remainder n 2))
      ;; si es par
      (/ (+ n 2) (+ n 1))
      ;; si es impar
      (/ (+ n 1) (+ n 2))
      )
  )
;; Ejemplos:
;;(factor-wallis 1)
;; = 2/3
;;(factor-wallis 5)
;; = 6/7
;;;;

;;
;; Nombre: wallis-iterativa
;; Objetivo: aproxima el valor de pi/4 con el producto de Wallis
;; Parámetros:
;;   n: número de factores a usar para la aproximación
;; Resultados:
;;   Valor de la aproximación de pi/4 según el número de términos usados
;; Descripción:
;;   La aproximación de pi/4 por el producto de Wallis se obtiene multiplicando
;;   tantos factores del productorio como pida el usuario
;; Funciones a las que llama: factor-wallis
;;
(define (wallis-iterativa n)
  (do (
       (i 1 (+ i 1))
       (resultado 1.0 (* resultado (factor-wallis i)))
       )
    ((> i n) resultado)
    )
  )

;;
;; Nombre: wallis-recursiva
;; Objetivo: aproxima el valor de pi/4 con el producto de Wallis
;; Parámetros:
;;   cota: define el rango en torno a 1 a partir del que se para el productorio
;; Resultados:
;;   Valor de la aproximación de pi/4 según la cota de error recibida
;; Descripción:
;;   La aproximación de pi/4 por el producto de Wallis se obtiene multiplicando
;;   factores hasta que uno quede dentro del rango definido por la cota
;; Funciones a las que llama: factor-wallis
;;
(define (wallis-recursiva cota)
  (define (auxiliar cota producto n)
    (let* (
          (factor (factor-wallis n))
          (prod-nuevo (* producto factor))
          )
      (if (< (- 1 cota) factor (+ 1 cota))
          prod-nuevo
          (auxiliar cota prod-nuevo (+ n 1))
          )
      )
    )
  (auxiliar cota 1.0 1)
  )
;; Ejemplos:
;;(wallis-recursiva 0.01)
;; = 0.781519...
;;(wallis-recursiva 0.001)
;; = 0.785005...
;;(wallis-recursiva 0.0001)
;; = 0.785358...
;;;;

;;;;
;; Ejercicio 11
;;;;

;;
;; Nombre: incremento-funcional
;; Objetivo:
;;   Devuelve una función que calcula el incremento funcional para la función recibida
;; Parámetros:
;;   f: función sobre la que se realizan los cálculos
;; Resultados:
;;   Función que permite calcula el incremento funcional para la función recibida
;;   como parámetro para un valor de x determinado
;; Descripción:
;;   La función devuelta permite que se calcule el incremento funcional para cualquier
;;   punto de la función que se recibe como parámetro.
;;
(define (incremento-funcional f)
  (lambda (x)
    (/
     (+ (f (+ x 1)) (* -2 (f x)) (f (- x 1)))
     4
     )
    )
  )
;; Ejemplos:
;;(define incr_y=x (incremento-funcional (lambda (x) x)))
;;(incr_y=x 5)
;; = 0
;;;;

;;;;
;; Ejercicio 12
;;;;

;;
;; Nombre: diferencia-simetrica
;; Objetivo: obtener una función que dé la diferencia simétrica para los parámetros
;; Parámetros:
;;   f: función a la que se hace la diferencia
;;   g: función que resta a la otra
;; Resultados:
;; 