/*
  Fichero: "ej3_trajadores.pl"
  Descripción: Ejercicio 3
 */
 
/*
  encargado_de_tarea(Trabajador, Area)
  
  Predicado que indica las tareas de las que se encarga un trajador
  
  Argumento
  + Trabajador
  
*/
encargado_de_tarea(miguel, admision).
encargado_de_tarea(miguel, control).
encargado_de_tarea(miguel, vigilancia).
encargado_de_tarea(ricardo, planificacion).
encargado_de_tarea(ricardo, asesoramiento).
encargado_de_tarea(alicia, direccion).
encargado_de_tarea(alicia, control).

encargada(Tarea) :-
	encargado_de_tarea(_, Tarea).
	
comparten_tarea(Persona1, Persona2) :-
	encargado_de_tarea(Persona1, Tarea),
	encargado_de_tarea(Persona2, Tarea).
