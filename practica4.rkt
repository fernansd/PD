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
;; Resultado:
;;   Devuelve el producto escalar de los vectores
;; Descripción:
;;   El producto escalar se calcula sumando el producto de las componentes de un
;;   vector con las correspondientes en otro vector
;;
(define (producto-escalar v1)
  (foldl (lambda (producto e1 e2) (+ producto (* e1 e2)))
         0 v1 v2)
  )