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
(define (crear-asignatura-vacia)
  ;; Una asignatura es una lista de estudiantes
  '()
  )

;;
;; Funciones de consulta
(define (asignatura-vacia? asignatura)
  (null? asignatura)
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
;; Nombre: insertar-estudiante!
;; Resultado: inserta por orden alfabético el estudiante en la lista
;; Descripción: si el estudiante ya existe no cambia nada, si no lo inserta en su sitio
;;
(define (insertar-estudiante! asignatura estudiante)
  ;; Comprueba si existe el estudiante
      (cond
        ;; Comprueba si no hay estudiantes en la lista para insertar directamente
        ((null? asignatura)
         (cons estudiante asignatura)
         )
        ;; Comprueba si sus apellidos son menores
        ((string-ci<? (ver-apellidos estudiante) (ver-apellidos (car asignatura)))
         (cons estudiante asignatura)
         )
        ;; En caso de tener los mismos apellidos comprueba si es menor por nombre
        ((and
          (string-ci=? (ver-apellidos estudiante) (ver-apellidos (car asignatura)))
          (string-ci<=? (ver-nombre estudiante) (ver-nombre (car asignatura)))
          )
         (if (string-ci=? (ver-nombre estudiante) (ver-nombre (car asignatura)))
             (begin (display "hola")asignatura)
             (cons estudiante asignatura)
             )
         )
        ;; En caso de no poder insertarlo, pasa a comparar con el siguiente
        (else
         (cons (car asignatura) (insertar-estudiante! (cdr asignatura) estudiante))
         )
        )
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
    ((null? asignatura) '())
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
  (map (lambda (estudiante) (newline)(consultar-estudiante estudiante)) asignatura)
  (newline)
  )

(define (consultar-estudiantes-tipo asignatura tipo)
  (cond
    ((null? asignatura));; Termina la ejecución
    ((string-ci=? tipo (ver-tipo (car asignatura)))
     (consultar-estudiante (car asignatura))
     )
    )
  )

(define asig (insertar-estudiante! '() (crear-estudiante "ana" "salas" "propio")))
(define asig1 (insertar-estudiante! asig (crear-estudiante "ana" "perez" "erasmus")))
(define asig2 (insertar-estudiante! asig1 (crear-estudiante "paco" "perez" "propio")))
(define asig3 (insertar-estudiante! asig2 (crear-estudiante "paco" "zamora" "propio")))
(modificar-estudiante! asig3 (crear-estudiante "PaCo" "pERez" "propio") (crear-estudiante "pepe" "xiez" "erasmus"))
(consultar-estudiantes asig3)
(display "-----------\n")
(consultar-estudiantes-tipo asig3 "erasmus")