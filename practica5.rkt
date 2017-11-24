;;
;; Fichero: "practica5.rkt"
;; Descripción: Ejercicios de la práctica 5
;; Autor: Fernando Sánchez Delgado
;;

;;;;
;; Ejercicio 1: Tipo Estudiante
;;;;

;;
;; Función de creación
(define (crear-estudiante nombre apellidos tipo)
  (list
   (list 'nombre nombre)
   (list 'apellidos apellidos)
   (list 'tipo tipo)
   )
  )

;;
;; Funciones de consulta
(define (ver-nombre estudiante)
  (cadr (assoc 'nombre estudiante))
  )

(define (ver-apellidos estudiante)
  (cadr (assoc 'apellidos estudiante))
  )

(define (ver-tipo estudiante)
  (cadr (assoc 'tipo estudiante))
  )

;;
;; Funciones de modificación
(define (cambiar-nombre! estudiante nuevo)
  (set-cdr! (assoc 'nombre estudiante) (list nuevo))
  )

(define (cambiar-apellidos! estudiante nuevo)
  (set-cdr! (assoc 'apellidos estudiante) (list nuevo))
  )

(define (cambiar-tipo! estudiante nuevo)
  (set-cdr! (assoc 'tipo estudiante) (list nuevo))
  )

;;;;
;; Ejercicio 2: Tipo Asignatura
;;;;

;;
;; Función de creación

;; Crea una asignatura sin nombre ni estudiantes
(define (crear-asignatura-vacia)
  ;; Una asignatura es una lista de estudiantes y su nombre
  (list
   (list 'nombre "")
   (list 'estudiantes '())
   )
  )


(define (crear-asignatura nombre)
  (list
   (list 'nombre nombre)
   (list 'estudiantes '())
   )
  )

;;
;; Funciones de consulta
;;
(define (ver-estudiantes asignatura)
  (cadr (assoc 'estudiantes asignatura))
  )

(define (ver-nombre-asig asignatura)
  (cadr (assoc 'nombre asignatura))
  )

(define (asignatura-vacia? asignatura)
  (null? (ver-estudiantes asignatura))
  )

;;
;; Nombre: buscar-estudiante
;; Resultado: Devuelve el estudiante si lo encuentra, o #f en caso contrario
;;
(define (buscar-estudiante asignatura nombre apellidos)
  ;; Busca en la lista de estudiantes aquel con mismo nombre apellidos
  ;; Devuelve #f si no lo encuentra
  (findf
   ;; Predicado que comprueba si coinciden los nombre y apellidos
   (lambda (estudiante)
     (and
      (string-ci=? nombre (ver-nombre estudiante))
      (string-ci=? apellidos (ver-apellidos estudiante))
      )
     )
   asignatura
   )
  )

;;
;; Funciones de modificación
;;

;; Cambia el nombre de la asignatura por el recibido
(define (cambiar-nombre-asig! asignatura nuevo)
  (set-car! (cadr (assoc 'nombre asignatura)) nuevo)
  )

;; Cambia la lista de estudiantes que pertenecen a la asignatura
(define (cambiar-estudiantes! asignatura lista-estudiantes)
  (set-car! (cadr (assoc 'estudiantes asignatura)) lista-estudiantes)
  )

;;
;; Nombre: insertar-estudiante!
;; Resultado: inserta por orden alfabético el estudiante en la lista
;; Descripción: si el estudiante ya existe no cambia nada, si no lo inserta en su sitio
;;
(define (insertar-estudiante! asignatura estudiante)
  (define (auxiliar lista-estudiantes estudiante)
    ;; Comprueba si existe el estudiante
    (cond
      ;; Comprueba si no hay estudiantes en la lista para insertar directamente
      ((null? lista-estudiantes)
       (cons estudiante lista-estudiantes)
       )
      ;; Comprueba si sus apellidos son menores
      ((string-ci<? (ver-apellidos estudiante) (ver-apellidos (car lista-estudiantes)))
       (cons estudiante lista-estudiantes)
       )
      ;; En caso de tener los mismos apellidos comprueba si es menor por nombre
      ((and
        (string-ci=? (ver-apellidos estudiante) (ver-apellidos (car lista-estudiantes)))
        (string-ci<=? (ver-nombre estudiante) (ver-nombre (car lista-estudiantes)))
        )
       (if (string-ci=? (ver-nombre estudiante) (ver-nombre (car lista-estudiantes)))
           (begin (display "hola") lista-estudiantes)
           (cons estudiante lista-estudiantes)
           )
       )
      ;; En caso de no poder insertarlo, pasa a comparar con el siguiente
      (else
       (cons (car asignatura) (auxiliar (cdr asignatura) estudiante))
       )
      )
    )
  (set-cdr! (assoc 'estudiantes asignatura) (list (auxiliar (ver-estudiantes asignatura) estudiante)))
  (display asignatura)
  )

(define (borrar-estudiante! asignatura estudiante)
  (cond
    ((null? asignatura) '())
    ((and
      (string-ci=? (ver-apellidos estudiante) (ver-apellidos (car asignatura)))
      (string-ci=? (ver-nombre estudiante) (ver-nombre (car asignatura)))
      )
     (cdr asignatura)
     )
    (else
     (cons (car asignatura) (borrar-estudiante! (cdr asignatura) estudiante))
     )
    )
  )

;;
;; Nombre: modificar-estudiante!
;; Resultado: asignatura con los datos del estudiante cambiados
;; Descripción: busca el estudiante objetivo y lo sustituye por nuevo
;;
(define (modificar-estudiante! asignatura objetivo nuevo)
  (cond
    ((asignatura-vacia? asignatura) '())
    (else
     (insertar-estudiante! (borrar-estudiante! asignatura objetivo) nuevo)
     )
    )
  )

;;
;; Nombre: consultar-estudiante
;; Descripción: imprime los datos del estudiante recibido
;;
(define (consultar-estudiante estudiante)
  (display "Nombre: ")(display (ver-nombre estudiante))(newline)
  (display "Apellidos: ")(display (ver-apellidos estudiante))(newline)
  (display "Tipo: ")(display (ver-tipo estudiante))(newline)
  )

(define (consultar-estudiantes asignatura)
  (display "Estudiantes de la asignatura:")(newline)
  (map (lambda (estudiante) (newline)(consultar-estudiante estudiante)) (ver-estudiantes asignatura))
  (newline)
  )

(define (consultar-estudiantes-tipo asignatura tipo)
  (define (auxiliar lista-estudiantes)
    (cond
      ((asignatura-vacia? asignatura));; Termina la ejecución
      ((string-ci=? tipo (ver-tipo (car lista-estudiantes)))
       (consultar-estudiante (car lista-estudiantes))
       )
      )
    )
  (auxiliar (ver-estudiantes asignatura))
  )


(define asignatura (crear-asignatura-vacia))
(insertar-estudiante! asignatura (crear-estudiante "ana" "salas" "propio"))
(display "1\n")
(insertar-estudiante! asignatura (crear-estudiante "ana" "perez" "erasmus"))
(display "2\n")
(insertar-estudiante! asignatura (crear-estudiante "paco" "perez" "propio"))
(display "3\n")
(insertar-estudiante! asignatura (crear-estudiante "paco" "zamora" "propio"))
(display "4\n")
;(display (borrar-estudiante! asig3 (crear-estudiante "PaCo" "pERez" "propio")))
(display "\n##------------\n")
;(modificar-estudiante! asig3 (crear-estudiante "PaCo" "pERez" "propio") (crear-estudiante "pepe" "xiez" "erasmus"))
(consultar-estudiantes asignatura)
(display "-----------\n")
(consultar-estudiantes-tipo asignatura "erasmus")