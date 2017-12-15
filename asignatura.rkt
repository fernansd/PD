;;
;; Fichero: "asignatura.rkt"
;; Descripción: Ejercicios de la práctica 5
;; Autor: Fernando Sánchez Delgado
;;

(require racket/list)

;;;;
;; Ejercicio 1: Tipo Estudiante
;;;;

;;
;; Función de creación
;;
(define (crear-estudiante nombre apellidos tipo)
  (list
   (list 'nombre nombre)
   (list 'apellidos apellidos)
   (list 'tipo tipo)
   )
  )

;;
;; Funciones de consulta
;;
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
;;
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
;; Funciones de creación
;;

;; Crea una asignatura sin estudiantes
(define (crear-asignatura-vacia)
  ;; Una asignatura es una lista vacía
  '()
  )

;; Crea una asignatura recibiendo como parámetro cero o más estudiantes
(define crear-asignatura
  (lambda lista-estudiantes
    lista-estudiantes
    )
  )

;;
;; Funciones de consulta
;;
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
      ;; Comprueba que coincidan el nombre y el apellido
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

;;
;; Nombre: insertar-estudiante!
;; Resultado: asignatura con el estudiante nuevo en la posición que corresponde
;; Descripción: inserta por orden alfabético el estudiante en la lista. Si el
;;   estudiante ya existe no cambia nada, si no, lo inserta en su sitio.
;;
(define (insertar-estudiante! asignatura estudiante)
  ;; Comprueba si existe el estudiante
  (cond
    ;; Comprueba si no hay estudiantes en la lista para insertar directamente
    ((asignatura-vacia? asignatura)
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
     ;; Comprueba si tienen el mismo nombre, para no insertarlo si ya existe
     (if (string-ci=? (ver-nombre estudiante) (ver-nombre (car asignatura)))
         asignatura
         (cons estudiante asignatura)
         )
     )
    ;; En caso de no poder insertarlo, pasa a comparar con el siguiente
    (else
     (cons (car asignatura) (insertar-estudiante! (cdr asignatura) estudiante))
     )
    )
  )

;;
;; Nombre: borrar-estudiante!
;; Resultado: asignatura con el estudiante recibido, eliminado de ella
;; Descripción: busca el estudiante y lo elimina de la asignatura, si no lo
;;     encuentra, no cambia nada.
;;
(define (borrar-estudiante! asignatura estudiante)
  (cond
    ;; Si la asignatura está vacía, la devuelve tal cual
    ((asignatura-vacia? asignatura) asignatura)
    ;; Si encuentra al estudiante, devuelve la asignatura si él
    ((and
      (string-ci=? (ver-apellidos estudiante) (ver-apellidos (car asignatura)))
      (string-ci=? (ver-nombre estudiante) (ver-nombre (car asignatura)))
      )
     (cdr asignatura)
     )
    ;; Si el estudiante no está en la cabeza de lista, llama de nuevo sobre el resto
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
    ;; Si la asignatura está vacía, la devuelve tal cual
    ((asignatura-vacia? asignatura) '())
    ;; En caso contrario, ejecuta la función borrar estudiante y lo inserta modificado
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

;;
;; Nombre: consultar-estudiantes
;; Descripción: imprime los datos de todos los estudiantes de la asignatura
;;
(define (consultar-estudiantes asignatura)
  (display "Estudiantes de la asignatura:")(newline)
  (map (lambda (estudiante) (newline)(consultar-estudiante estudiante)) asignatura)
  (newline)
  )

;;
;; Nombre: consultar-estudiantes-tipo
;; Descripción: imprime los datos de todos los estudiantes con el tipo proporcionado
;;
(define (consultar-estudiantes-tipo asignatura tipo)
  (cond
    ;; Si la asignatura está vacía lo comunica
    ((asignatura-vacia? asignatura)
     (newline)(display "La asignatura no tiene estudiantes\n"))
    ;; Si conincide el tipo del estudiante con el buscado, lo imprime
    ((string-ci=? tipo (ver-tipo (car asignatura)))
     (consultar-estudiante (car asignatura))
     (consultar-estudiantes-tipo (cdr asignatura) tipo)
     )
    )
  )

;;
;; Nombre: cmp-estudiantes
;; Resultado: devuelve -1 si el primero es menor, 0 si son iguales y 1 si es mayor
;; Descripción: compara los estudiantes por nombres y apellidos
;;
(define (cmp-estudiantes est1 est2)
  (cond
    ((string-ci<? (ver-apellidos est1) (ver-apellidos est2))
     -1 ;; Si el primero es menor devuelve -1
     )
    ((string-ci>? (ver-apellidos est1) (ver-apellidos est2))
     1 ;; Si el primero es mayor devuelve 1
     )
    ;; Si llega hasta aquí tienen los mismos apellidos, se comprueba el nombre
    ((string-ci<? (ver-nombre est1) (ver-nombre est2))
     -1 ;; Si el primero es menor devuelve -1
     )
    ((string-ci>? (ver-nombre est1) (ver-nombre est2))
     1 ;; Si el primero es mayor devuelve 1
     )
    ;; Si llega hasta aquí lo estudiantes son iguales, devuelve 0
    (else 0)
    )
  )

(define (ordenar-estudiantes! asignatura)
  (cond
    ;; Si no hay ningún elemento devuelve la lista
    ((null? asignatura) asignatura)
    ;; Si solo hay un elemento en la lista la devuelve
    ((= 1 (length asignatura)) asignatura)
    ;; En caso general
    (else
     (define pivote (list-ref asignatura (quotient (length asignatura) 2)))
     (do (
          (menores '() (if (= -1 (cmp-estudiantes (car lista) pivote))
                           (cons (car lista) menores)
                           menores)
                   )
          (mayores '() (if (= 1 (cmp-estudiantes (car lista) pivote))
                           (cons (car lista) mayores)
                           mayores)
                   )
          (lista asignatura (cdr lista))
          )
       ((null? lista) (append
                       (ordenar-estudiantes! menores)
                       (list pivote)
                       (ordenar-estudiantes! mayores)
                       )
                      )
       )
     )
    )
  )

(define (grabar-estudiantes asignatura nombre-fichero)
  (if (file-exists? nombre-fichero)
      #f ;; Si ya existe el fichero devuelve false
      (do (
           (puerto (open-output-file nombre-fichero))
           (lista-aux asignatura (cdr lista-aux))
           )
        ((null? lista-aux)
         ;; Cuando termina cierra el puerto de salida
         (close-output-port puerto)
         )
        ;; Imprime el donante en la cabeza al fichero
        (display (ver-nombre (car lista-aux)) puerto)
        (display " " puerto)
        (display (ver-apellidos (car lista-aux)) puerto)
        (display " " puerto)
        (display (ver-tipo (car lista-aux)) puerto)
        (display " " puerto)
        (newline puerto)
        )
      )
  )

;; Función auxiliar que pone comillas al principio y final de una cadena
(define (poner-comillas cadena)
  (string-append "#\"" cadena "#\"")
  )

(define (cargar-fichero-lista-estudiantes nombre-fichero)
  (cond
    ((not (file-exists? nombre-fichero))
     #f ;; Devuelve false si no existe el fichero
     )
    (else
     (define puerto (open-input-file nombre-fichero))
     (do (
          (lista-estudiantes '() (append lista-estudiantes
                                         (list (crear-estudiante (symbol->string nombre)
                                                                 (symbol->string apellido)
                                                                 (read puerto) ;; lee tipo
                                                                 )
                                               )
                                         )
                             )
          ;; Al leer el nombre desde un fichero, le quita las comillas
          (nombre (read puerto) (read puerto))
          (apellido (read puerto) (read puerto))
          )
       ;; Condicion de salida del bucle
       ((eof-object? nombre) 
        ;; Al acabar cerramos el puerto de salida y devuelve la lista
        (close-input-port puerto) 
        lista-estudiantes                  
        )
       )
     )
    )
  )

(define (programa)
  
  ;;
  ;; Funciones auxiliares
  ;;
  
  ;; Función que pide una opción al usuario
  (define (pedir-opcion)
    (newline)
    (display "Elige una opcion:" )
    (newline)
    (display "1) Crear una asignatura")
    (newline)
    (display "2) Comprobar si la asignatura está vacía")
    (newline)
    (display "3) Insertar un estudiante")
    (newline)
    (display "4) Buscar un estudiante")
    (newline)
    (display "5) Modificar un estudiante")
    (newline)
    (display "6) Borrar un estudiante")
    (newline)
    (display "7) Consultar un estudiante")
    (newline)
    (display "8) Consultar los estudiantes")
    (newline)
    (display "9) Consultar los estudiante de un tipo")
    (newline)
    (display "10) Ordenar los estudiantes por apellidos y nombre")
    (newline)
    (display "11) Grabar los estudiantes en un fichero")
    (newline)
    (display "12) Cargar los estudiantes de un fichero")
    (newline)
    (display "0) Salir" )
    (newline)
    
    (newline)
    (display " --> ")
    ;; Lee la opción
    (read)
    )
  
  ;; 
  ;; Descripción: función que lee una variable de teclado
  ;;
  (define (leer-teclado mensaje)
    (display mensaje)
    (display " --> ")
    (read)
    )
  
  ;;
  ;; Descripción: función que lee una cadena de teclado con espacios
  ;;
  (define (leer-teclado-cadena mensaje)
    (display mensaje)
    (display " --> ")
    
    ;; Elimina el carácter de salto de línea #\newline, si existe
    (if (char=? (peek-char) #\newline)
        (read-char)
        )
    
    ;; Lee los caracteres hasta que encuentra el carácter de salto de línea #\newline
    (do
        (
         (cadena   (make-string 0)  (string-append cadena (string caracter)))
         (caracter (read-char)      (read-char))
         )
      ((char=? #\newline caracter) 
       ;; Devuelve la cadena
       cadena
       )
      )
    )
  
  (define (leer-estudiante-teclado)
    (crear-estudiante
     (leer-teclado-cadena "Nombre del estudiante (poner comillas): ")
     (leer-teclado-cadena "Apellidos (poner comillas): ")
     (leer-teclado "Tipo: ")
     )
    )
  
  ;;;;
  ;; Cuerpo principal del programa
  ;;;;
  
  ;; Asignatura que gestionará el programa
  (define asignatura (crear-asignatura-vacia))
  
  ;; Bucle principal
  (do (
       (opcion (pedir-opcion) (pedir-opcion))
       )
    ((= 0 opcion) (display "Fin del programa\n"))
    (cond
      ;; Crea una asignatura
      ((= 1 opcion)
       (display "Se borrará la anterior asignatura.")
       (set! asignatura (crear-asignatura-vacia))
       )
      ;; Comprobar si la asignatura está vacía
      ((= 2 opcion)
       (if (asignatura-vacia? asignatura)
           (display "La asignatura está vacía.\n")
           (display "La asignatura no está vacía.\n")
           )
       )
      ;; Insertar estudiante
      ((= 3 opcion)
       (set! asignatura (insertar-estudiante! asignatura (leer-estudiante-teclado)))
       (display "Estudiante insertado.\n")
       )
      ;; Buscar un estudiante
      ((= 4 opcion)
       (display "Estudiante a buscar\n")
       (define estudiante
         (buscar-estudiante asignatura
                          (leer-teclado-cadena "Nombre del estudiante (poner comillas): ")
                          (leer-teclado-cadena "Apellidos (poner comillas): ")
                          )
         )
       (display "Resultado:\n")
       (if estudiante
           estudiante
           (display "No se ha encontrado\n")
           )
       )
      ;; Modifica un estudiante
      ((= 5 opcion)
       (display "Estudiante a modificar\n")
       (define estudiante
         (buscar-estudiante asignatura
                          (leer-teclado-cadena "Nombre del estudiante (poner comillas): ")
                          (leer-teclado-cadena "Apellidos (poner comillas): ")
                          )
         )
       (display "Resultado:\n")
       (define nuevo (leer-estudiante-teclado))
       (if estudiante
           (set! asignatura (modificar-estudiante! asignatura estudiante nuevo))
           (display "No se ha encontrado\n")
           )
       ;; Devuelve la asignatura
       asignatura
       )
      ;; Borrar un estudiante
      ((= 6 opcion)
       (display "Estudiante a borrar\n")
       (define estudiante
         (buscar-estudiante asignatura
                          (leer-teclado-cadena "Nombre del estudiante (poner comillas): ")
                          (leer-teclado-cadena "Apellidos (poner comillas): ")
                          )
         )
       (if estudiante
           (set! asignatura (borrar-estudiante! asignatura estudiante))
           (display "No se ha encontrado\n")
           )
       )
      ;; Consultar un estudiante
      ((= 7 opcion)
       (display "Estudiante a buscar\n")
       (define estudiante
         (buscar-estudiante asignatura
                          (leer-teclado-cadena "Nombre del estudiante (poner comillas): ")
                          (leer-teclado-cadena "Apellidos (poner comillas): ")
                          )
         )
       (display "Resultado:\n")
       (if estudiante
           (consultar-estudiante estudiante)
           (display "No se ha encontrado\n")
           )
       )
      ;; Consultar los estudiantes
      ((= 8 opcion)
       (display "Estudiantes de la asignatura:")
       (newline)
       (consultar-estudiantes asignatura)
       )
      ;; Consultar los estudiantes de un tipo
      ((= 9 opcion)
       (define tipo (leer-teclado "Introducir el tipo: "))
       (consultar-estudiantes-tipo asignatura tipo)
       )
      ;; Ordenar los estudiantes
      ((= 10 opcion)
       (set! asignatura (ordenar-estudiantes! asignatura))
       (newline)
       (display "Estudiantes ordenados.\n")
       )
      ;; Grabar en un fichero la asignatura
      ((= 11 opcion)
       (define nombre-fichero (leer-teclado-cadena "Introduzca el nombre del fichero: "))
       ;; Comprueba si se ha abierto correctamente
       (if nombre-fichero
           (grabar-estudiantes asignatura nombre-fichero)
           (display "No se ha podido crear el fichero, puede que ya exista\n")
           )
       )
      ;; Cargar de un fichero la asignatura
      ((= 12 opcion)
       (define nombre-fichero (leer-teclado-cadena "Introduzca el nombre del fichero: "))
       ;; Comprueba si se ha abierto correctamente
       (if nombre-fichero
           (set! asignatura (cargar-fichero-lista-estudiantes nombre-fichero))
           (display "El fichero no se ha podido abrir, puede que no exista\n")
           )
       )
      )
    )
  )

;; Ejecuta el programa
(programa)