;;
;; Fichero: "polinomio.rkt"
;; Descripción: Ejercicios de la práctica 5: Tipo Polinomio
;; Autor: Fernando Sánchez Delgado
;;

;;
;; Nombre: crear_polinomio
;; Parámetros:
;;   grado_i: entero que representa el grado del coeficiente
;;   coef_i: número real que representa el coeficiente para el grado que le sigue
;;   Ejemplo: x^2 + 3 == 0 3 2 1
;;   Nota: recibe un número variable de argumentos, pero siempre par
;; Resultado:
;;   Lista asociativa con los grados asociados a los coeficientes
;;
(define crear_polinomio
  (lambda lista
    (display lista)(newline)
    (if (null? lista)
        '() ;; Polínomio vacío
        ;; 
        (cons (list (car lista) (cadr lista)) (apply crear_polinomio (cddr lista)))
        )
    )
  )

(define (polinomio_vacio? polinomio)
  (null? polinomio)
  )

(define (grado polinomio)
  (if (polinomio_vacio? polinomio)
      -1
      (max (caar polinomio) (grado (cdr polinomio)))
      )
  )

(define (coeficiente polinomio grado)
  (cadr (assoc grado polinomio))
  )

(define (borrar_termino! polinomio grado)
  (cond
    ;; Comprueba si no queda elementos en la lista
    ((null? polinomio) '())
    ;; Si ha encontrado el monomio
    ((= grado (caar polinomio))
     (cdr polinomio)
     )
    ;; Si es un elemento normal
    (else
     (cons
      ;; Inserta de nuevo el término actual cuando acabe la búsqueda
      (car polinomio)
      ;; LLama la función de nuevo sobre el resto de la lista
      (borrar_termino! (cdr polinomio) grado)
      )
     )
    )
  )

(define (modificar_coeficiente! polinomio grado coef)
  (define monomio (assoc grado polinomio))
  (cond
    ;; Si el coeficiente es 0, se elimina el monomio
    ((= 0 coef)
     (borrar_termino polinomio grado)
     )
    ;; Si el monomio existe, se modifica el coeficiente
    (monomio
     (set-car! (cdr monomio) coef)
     )
    ;; Si no existe el monomio, se inserta
    (else
     (cons (list grado coef) polinomio)
     )
    )
  )

(define (valor_polinomio polinomio x)
  (if (polinomio_vacio? polinomio)
      0
      (+ (* (cadar polinomio) (expt x (caar polinomio))) (valor_polinomio (cdr polinomio) x))
      )
  )

(define (imprimir_monomio monomio)
  (let ((grado (car monomio))
        (coef (cadr monomio))
        )
    (cond
      ((= 0 grado)
       (display coef)
       )
      ((= 1 coef)
       (display "x^")(display grado)
       )
      (else
       (display coef)(display "x^")(display grado)
       )
      )
    )
  )

(define (imprimir_polinomio polinomio)
  (imprimir_monomio (car pol))
  (do
      (
       (restante (cdr pol) (cdr restante))
       )
    ((null? restante) (newline))
    ;; Cuerpo del bucle
    (if (> (cadar restante) 0)
             (display " +"))
    (display " ")
    (imprimir_monomio (car restante))
    )
  )

;(define (programa)
;  (define (pedir-opcion)
;    (display "Elige una opcion" )
;    (newline)
;    (display "1 -> Crear un polinomio")
;    (newline)
;    (display "2 -> Escribir polinomio")
;    (newline)
;    (display "3 -> Modificar un coeficiente del polinomio")
;    (newline)
;    (display "4 -> Aplicar polinomio a un número")
;    (newline)
;    (display "5 -> Grabar polinomio en un fichero")
;    (newline)
;    (display "6 -> Cargar polinomio desde un fichero")
;    (newline)
;    (display "0 -> Salir" )
;    (newline)
;    
;    (newline)
;    (display "--> ")
;    (read)
;    )
;  
;  (define (leer-teclado mensaje)
;    (display mensaje)
;    (display "--> ")
;    (let ((leido (read)))
;      (if (symbol? leido)
;          (symbol->string leido)
;          leido
;          )
;      )
;    )
;  
;  ;;;;
;  ;; Cuerpo del programa
;  ;;;;
;  (do
;      (;; Variables
;       (polinomio (crear_polinomio))
;       (opcion (pedir-opcion) (pedir-opcion))
;       )
;    ((= opcion 0) (display "Programa finalizado.\n"))
;    (cond
;      ;; 1. Crear un polinomio
;      ((= 1 opcion)
;       
;       )
;      ;; 2. Escribir polinomio
;      ((= 2 opcion)
;       
;       )
;      ;; 3. Modificar un coeficiente del polinomio
;      ((= 3 opcion)
;       (display "Modificicación de un coeficiente del polinomio:\n")
;       (let (
;             (grado (leer-teclado "Grado del monomio a modificar: "))
;             (nuevo-coef (leer-teclado "Nuevo valor del coeficiente: "))
;             )
;         (set! polinomio (modificar_coeficiente! polinomio grado nuevo-coef))
;         )
;       )
;      ;; 4. Aplicar polinomio a un número
;      ((= 4 opcion))
;      ;; 5. Grabar polinomio en un fichero
;      ((= 5 opcion))
;      ;; 6. Leer polinomio de un fichero
;      ((= 6 opcion))
;      (else
;       (display "Opción incorrecta\n")
;       )
;      )
;    )
;  
;  )

(define pol (crear_polinomio 0 3.1 2 4.2 5 8.9))
(grado pol)
(coeficiente pol 2)
(modificar_coeficiente! pol 0 9.1)
(display "-> ")(display pol)(newline)
(valor_polinomio pol 1)
(borrar_termino! pol 2)
(display pol)(newline)
(imprimir_polinomio pol)