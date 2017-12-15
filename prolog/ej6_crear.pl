/*
  Fichero: "ej6_crear.pl"
  Descripción: Ejercicio 6
 */

crear(N, L):-
	crear(0, N, L),!.
	
crear(N, N, [N]).

crear(N, Limite, [N|L]):-
	N1 is N+1,
	crear(N1, Limite, L).
	
crear(N, Limite, Paso, [N|L]):-
	N < Limite,
	N1 is N + Paso,
	crear(N1, Limite, Paso, L).
