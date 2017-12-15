/*
  Fichero: "ej7_primos.pl"
  Descripción: Ejercicio 7
 */

primo(2).
 
primo(N):-
	N > 2,
	.
	
divisible(N, D):-
	Resto is N mod D,
	Resto = 0.
	
divisores(N):-
	
	
crear_primos(N, []):-
	N < 1.

crear_primos(N, L):-
	
