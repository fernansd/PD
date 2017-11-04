;;
;; Fichero: "practica4.rkt"
;; Descripción: Ejercicios de la práctica 4
;; Autor: Fernando Sánchez Delgado
;;

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