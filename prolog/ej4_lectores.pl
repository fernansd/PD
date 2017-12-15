z/*
  Fichero: "ej4_lectores.pl"
  Descripción: Ejercicio 4
 */

lector(nombre("Ana", "Garrido", "Aguirre"), mujer, 31).
lector(nombre("Marta", "Cantero", "Lasa"), mujer, 20).
lector(nombre("Rodrigo", "Duque", "Soto"), hombre, 30).
lector(nombre("Pedro", "Garrido", "Aguirre"), hombre,20).
lector(nombre("Ana", "Sanz", "Lopez"), mujer, 28).

/* Preguntas en Prolog:
	¿Hay lectores?
		lector(_, _, _), !.
	
	¿Quiénes son lectores?
		lector(Nombre, _, _).
		
	¿Qué lectores son mujeres?¿Y hombres?
		lector(Nombre, mujer, _).
		lector(Nombre, hombre, _).
		
	¿Hay lectores con el mismo nombre y diferentes apellidos?
		lector(nombre(Nombre, A1, A2), _,_), lector(nombre(Nombre, A3, A4),_,_),
			(A1\=A3; A2\=A4), !.
 */

contar([], 0). 
contar([_|Cola], N) :-
	contar(Cola, N1),
	N is N1 + 1.

apellidos_repetidos(Apellido1, Apellido2) :-
	bagof(Nombre, N^S^E^lector(nombre(Nombre, Apellido1, Apellido2),S,E), Lista),
	contar(Lista, N), N > 1.
